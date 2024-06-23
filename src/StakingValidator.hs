{-# OPTIONS_GHC -Wno-unused-imports #-}

module StakingValidator where

import PlutusLedgerApi.V1 (Address (..), Credential (..), DatumHash, PubKeyHash (..), ScriptHash, StakingCredential (..))
import PlutusLedgerApi.V1.Value (AssetClass, CurrencySymbol (..), TokenName (..))
import PlutusLedgerApi.V2 (Redeemer)
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
import Plutarch.Num (PNum (pnegate))
import Plutarch.Prelude
import Plutarch.TryFrom (PTryFrom (PTryFromExcess, ptryFrom'))
import Plutarch.Unsafe
import "liqwid-plutarch-extra" Plutarch.Extra.Numeric ((#^))
import "liqwid-plutarch-extra" Plutarch.Extra.Rational ((#%))
import "liqwid-plutarch-extra" Plutarch.Extra.ScriptContext (pfromPDatum, ptryFromInlineDatum)
import "liqwid-plutarch-extra" Plutarch.Extra.TermCont

import BatchValidator (PSmartRedeemer (..))
import Constants (routerFeeAsNegativeLovelace)
import Plutarch.Builtin (PIsData (pdataImpl), ppairDataBuiltin)
import SingleValidator (PSmartHandleDatum (..))
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
  Term s (PMaybeData PAddress :--> PData :--> PDatum :--> PBool :--> PScriptContext :--> PBool) ->
  Term s (PMap any PDatumHash PDatum) ->
  Term s PScriptContext ->
  Term s PAddress ->
  Term s PInteger ->
  Term s (PBuiltinList (PBuiltinPair (PAsData PTxOut) (PAsData PBool))) ->
  Term s (PBuiltinList PTxOut) ->
  Term s PInteger
pfoldCorrespondingUTxOs validateFn datMap ctx swapAddress acc la lb =
  pfoldl2
    # plam
      ( \acc_ utxoInSwapFlagPair utxoOut ->
          acc_ + psmartHandleSuccessor validateFn datMap ctx swapAddress utxoInSwapFlagPair utxoOut
      )
    # acc
    # la
    # lb

psmartHandleSuccessor ::
  Term s (PMaybeData PAddress :--> PData :--> PDatum :--> PBool :--> PScriptContext :--> PBool) ->
  Term s (PMap any PDatumHash PDatum) ->
  Term s PScriptContext ->
  Term s PAddress ->
  Term s (PBuiltinPair (PAsData PTxOut) (PAsData PBool)) ->
  Term s PTxOut ->
  Term s PInteger
psmartHandleSuccessor validateFn datums ctx swapAddress smartInputSwapFlagPair swapOutput = P.do
  let smartInput = pfstBuiltin # smartInputSwapFlagPair
      forSwap = pfromData $ psndBuiltin # smartInputSwapFlagPair
  smartInputF <- pletFields @'["address", "value", "datum"] smartInput
  swapOutputF <- pletFields @'["address", "value", "datum"] swapOutput

  let smartInputDatum = pconvertChecked @PSmartHandleDatum $ presolveDatumData # smartInputF.datum # datums
      swapOutputDatum = presolveDatum # swapOutputF.datum # datums

  pif
    ( pand'List
        [ ptraceIfFalse "Incorrect Swap Address" (swapOutputF.address #== swapAddress)
        , pmatch smartInputDatum $ \case
            PSimple ((pfield @"owner" #) -> owner) ->
              pand'List
                [ validateFn # pcon (PDJust $ pdcons # pdata owner # pdnil) # punsafeCoerce (pconstant ()) # swapOutputDatum # pcon PTrue # ctx
                , ptraceIfFalse "Incorrect Swap Output Value" (pvalueHasChangedByLovelaces # smartInputF.value # swapOutputF.value # routerFeeAsNegativeLovelace)
                ]
            PAdvanced dat' -> P.do
              datF <- pletFields @'["mOwner", "routerFee", "reclaimRouterFee", "extraInfo"] dat'
              let routerFee = pif forSwap (pnegate # datF.routerFee) (pnegate # datF.reclaimRouterFee)
              pand'List
                [ validateFn # datF.mOwner # datF.extraInfo # swapOutputDatum # forSwap # ctx
                , ptraceIfFalse "Incorrect Swap Output Value" (pvalueHasChangedByLovelaces # smartInputF.value # swapOutputF.value # routerFee)
                ]
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

smartHandleStakeValidatorW :: Term s ((PMaybeData PAddress :--> PData :--> PDatum :--> PBool :--> PScriptContext :--> PBool) :--> PAddress :--> PStakeValidator)
smartHandleStakeValidatorW = phoistAcyclic $ plam $ \validateFn swapAddress redeemer ctx -> P.do
  let red = pconvertUnsafe @PRouterRedeemer redeemer
  redF <- pletFields @'["inputIdxs", "outputIdxs", "advancedRedeemers"] red
  ctxF <- pletFields @'["txInfo", "purpose"] ctx
  infoF <- pletFields @'["inputs", "outputs", "signatories", "redeemers", "datums"] ctxF.txInfo
  txInputs <- plet infoF.inputs
  txOuts <- plet infoF.outputs

  -- let smartInputs = puniqueOrdered # plam (\idx -> pfield @"resolved" #$ pelemAt @PBuiltinList # idx # txInputs) # 0 # redF.inputIdxs
  let smartInputs = puniqueOrdered # plam (\idx -> pelemAt @PBuiltinList # idx # txInputs) # 0 # redF.inputIdxs
      swapOutputs = puniqueOrdered # plam (\idx -> pelemAt @PBuiltinList # idx # txOuts) # 0 # redF.outputIdxs
      rdmrs = pto $ pfromData infoF.redeemers
      filteredRdmrs =
        pfilter
          # plam
            ( \prPair ->
                let purpose = pfromData $ pfstBuiltin # prPair
                 in pmatch purpose $ \case
                      PSpending _ -> pcon PTrue
                      _ -> pcon PFalse
            )
          # rdmrs
      inUTxOSwapFlagPairs =
        pzipWith
          # plam
            ( \smartInput prPair -> P.do
                let purpose = pfromData $ pfstBuiltin # prPair
                    r = pfromData $ psndBuiltin # prPair
                    smartRedeemer = pconvertUnsafe @PSmartRedeemer (pto r)
                inputF <- pletFields @'["outRef", "resolved"] smartInput
                -- inputRef = pfield @"outRef" # smartInput
                PSpending ((pfield @"_0" #) -> rdmrRef) <- pmatch purpose
                pif
                  (inputF.outRef #== rdmrRef)
                  ( pmatch smartRedeemer $ \case
                      PSwapSmart _ ->
                        ppairDataBuiltin # inputF.resolved # pdata (pcon PTrue)
                      PReclaimSmart _ ->
                        ppairDataBuiltin # inputF.resolved # pdata (pcon PFalse)
                  )
                  perror
            )
          # smartInputs
          # filteredRdmrs
      foldCount =
        pfoldCorrespondingUTxOs
          validateFn
          infoF.datums
          ctx
          swapAddress
          0
          inUTxOSwapFlagPairs
          swapOutputs

  let scInpCount = pcountScriptInputs # txInputs
      foldChecks =
        pand'List
          [foldCount #== scInpCount] -- possibly add protocol fee payout
  pif foldChecks (popaque $ pconstant ()) perror
