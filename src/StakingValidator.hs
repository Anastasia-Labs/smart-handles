{-# OPTIONS_GHC -Wno-unused-imports #-}

module StakingValidator where

import PlutusLedgerApi.V1 (Address (..), Credential (..), DatumHash, PubKeyHash (..), ScriptHash, StakingCredential (..))
import PlutusLedgerApi.V1.Value (AssetClass, CurrencySymbol (..), TokenName (..))
import PlutusTx qualified

import Plutarch.Api.V1 (PCredential (PPubKeyCredential, PScriptCredential), PDatumHash)
import Plutarch.Api.V1.AssocMap qualified as AssocMap
import Plutarch.Api.V1.Value
import Plutarch.Api.V1.Value qualified as Value
import Plutarch.Api.V2
import Plutarch.Bool
import Plutarch.DataRepr
import Plutarch.Lift (DerivePConstantViaBuiltin, PConstantDecl, PUnsafeLiftDecl (..))
import Plutarch.Monadic qualified as P
import Plutarch.Prelude
import Plutarch.TryFrom (PTryFrom (PTryFromExcess, ptryFrom'))
import Plutarch.Unsafe
import "liqwid-plutarch-extra" Plutarch.Extra.Numeric ((#^))
import "liqwid-plutarch-extra" Plutarch.Extra.Rational ((#%))
import "liqwid-plutarch-extra" Plutarch.Extra.ScriptContext (pfromPDatum, ptryFromInlineDatum)
import "liqwid-plutarch-extra" Plutarch.Extra.TermCont

import SingleValidator (PSmartHandleDatum (..))
import BatchValidator (PSmartRedeemer (..))
import Constants (routerFeeAsNegativeLovelace)
import Conversions
import Utils

pcountScriptInputs :: Term s (PBuiltinList PTxInInfo :--> PInteger)
pcountScriptInputs =
  phoistAcyclic $
    let go :: Term s (PInteger :--> PBuiltinList PTxInInfo :--> PInteger)
        go = pfix #$ plam $ \self n ->
          pelimList
            ( \x xs ->
                let cred = pfield @"credential" # (pfield @"address" # (pfield @"resolved" # x))
                 in pmatch cred $ \case
                      PScriptCredential _ -> self # (n + 1) # xs
                      _ -> self # n # xs
            )
            n
     in go # 0

data RouterRedeemer = RouterRedeemer
  { inputIdxs :: [Integer]
  , outputIdxs :: [Integer]
  }

PlutusTx.makeLift ''RouterRedeemer
PlutusTx.makeIsDataIndexed ''RouterRedeemer [('RouterRedeemer, 0)]

data PRouterRedeemer (s :: S)
  = PRouterRedeemer
      ( Term
          s
          ( PDataRecord
              '[ "inputIdxs" ':= PBuiltinList (PAsData PInteger)
               , "outputIdxs" ':= PBuiltinList (PAsData PInteger)
               ]
          )
      )
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PDataFields)

instance DerivePlutusType PRouterRedeemer where
  type DPTStrat _ = PlutusTypeData

instance PTryFrom PData PRouterRedeemer

instance PUnsafeLiftDecl PRouterRedeemer where type PLifted PRouterRedeemer = RouterRedeemer
deriving via (DerivePConstantViaData RouterRedeemer PRouterRedeemer) instance PConstantDecl RouterRedeemer

pfoldCorrespondingUTxOs ::
  Term s (PAddress :--> PDatum :--> PBool) ->
  Term s (PMap any PDatumHash PDatum) ->
  Term s PAddress ->
  Term s PInteger ->
  Term s (PBuiltinList PTxOut) ->
  Term s (PBuiltinList PTxOut) ->
  Term s PInteger
pfoldCorrespondingUTxOs validateFn datMap swapAddress acc la lb =
  pfoldl2
    # plam
      ( \acc_ utxoIn utxoOut ->
          acc_ + psmartHandleSuccessor validateFn datMap swapAddress utxoIn utxoOut
      )
    # acc
    # la
    # lb

psmartHandleSuccessor ::
  Term s (PAddress :--> PDatum :--> PBool) ->
  Term s (PMap any PDatumHash PDatum) ->
  Term s PAddress ->
  Term s PTxOut ->
  Term s PTxOut ->
  Term s PInteger
psmartHandleSuccessor validateFn datums swapAddress smartInput swapOutput = P.do
  smartInputF <- pletFields @'["address", "value", "datum"] smartInput
  swapOutputF <- pletFields @'["address", "value", "datum"] swapOutput

  let smartInputDatum = pconvert @PSmartHandleDatum $ presolveDatumData # smartInputF.datum # datums
      smartUser = pfield @"owner" # smartInputDatum
      swapOutputDatum = presolveDatum # swapOutputF.datum # datums

  pif
    ( pand'List
        [ ptraceIfFalse "Incorrect Swap Address" (swapOutputF.address #== swapAddress)
        , ptraceIfFalse "Incorrect Swap Output Value" (plovelaceValueOf # smartInputF.value + routerFeeAsNegativeLovelace #== plovelaceValueOf # swapOutputF.value)
        , validateFn # smartUser # swapOutputDatum
        ]
    )
    (pconstant 1)
    perror

puniqueOrdered :: (PElemConstraint PBuiltinList a) => Term s ((PInteger :--> a) :--> PInteger :--> (PBuiltinList (PAsData PInteger)) :--> (PBuiltinList a))
puniqueOrdered =
  phoistAcyclic $
    let go :: (PElemConstraint PBuiltinList a) => Term s ((PInteger :--> a) :--> PInteger :--> (PBuiltinList (PAsData PInteger)) :--> (PBuiltinList a))
        go = plam $ \elemAt ->
          ( pfix #$ plam $ \self uniquenessLabel ->
              pelimList
                ( \x xs ->
                    let n = 2 #^ (pfromData x)
                        n' = 2 * n
                        y = uniquenessLabel + n
                        output = elemAt # pfromData x
                     in pif
                          ((pmod # uniquenessLabel # n') #< (pmod # y # n'))
                          (pcons # output #$ self # y # xs)
                          (ptraceError "duplicate index detected")
                )
                (pcon PNil)
          )
     in go

smartHandleStakeValidatorW :: Term s ((PAddress :--> PDatum :--> PBool) :--> PAddress :--> PStakeValidator)
smartHandleStakeValidatorW = phoistAcyclic $ plam $ \validateFn swapAddress redeemer ctx -> P.do
  let red = punsafeCoerce @_ @_ @PRouterRedeemer redeemer
  redF <- pletFields @'["inputIdxs", "outputIdxs"] red
  ctxF <- pletFields @'["txInfo", "purpose"] ctx
  infoF <- pletFields @'["inputs", "outputs", "signatories", "datums"] ctxF.txInfo
  txInputs <- plet infoF.inputs
  txOuts <- plet infoF.outputs

  let smartInputs = puniqueOrdered # plam (\idx -> pfield @"resolved" #$ pelemAt @PBuiltinList # idx # txInputs) # 0 # redF.inputIdxs
      swapOutputs = puniqueOrdered # plam (\idx -> pelemAt @PBuiltinList # idx # txOuts) # 0 # redF.outputIdxs
      foldCount = pfoldCorrespondingUTxOs validateFn infoF.datums swapAddress 0 smartInputs swapOutputs

  let scInpCount = pcountScriptInputs # txInputs
      foldChecks =
        pand'List
          [foldCount #== scInpCount] -- possibly add protocol fee payout
  pif foldChecks (popaque $ pconstant ()) perror
