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
import Constants (negativeRouterFeeForSimpleRoutes, routerFeeForSimpleRoutes)
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

-- TODO: Switch to: '[ "indices" ':= PBuiltinList (PAsData (PBuiltinPair (PAsData PInteger) (PAsData PInteger)))
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
  deriving anyclass (PlutusType, PIsData, PDataFields, PShow)

instance DerivePlutusType PRouterRedeemer where
  type DPTStrat _ = PlutusTypeData

instance PTryFrom PData PRouterRedeemer

instance PUnsafeLiftDecl PRouterRedeemer where type PLifted PRouterRedeemer = RouterRedeemer
deriving via (DerivePConstantViaData RouterRedeemer PRouterRedeemer) instance PConstantDecl RouterRedeemer

pfoldCorrespondingUTxOs ::
  Term s PCustomValidator ->
  Term s (PMap any PDatumHash PDatum) ->
  Term s PScriptContext ->
  Term s PAddress ->
  Term s PInteger ->
  Term s (PBuiltinList (PBuiltinPair (PAsData PTxOut) (PAsData PBool))) ->
  Term s (PBuiltinList PTxOut) ->
  Term s PInteger
pfoldCorrespondingUTxOs validateFn datMap ctx routeAddress acc la lb =
  pfoldl2
    # plam
      ( \acc_ utxoInRouteFlagPair utxoOut ->
          acc_ + psmartHandleSuccessor validateFn datMap ctx routeAddress utxoInRouteFlagPair utxoOut
      )
    # acc
    # la
    # lb

psmartHandleSuccessor ::
  Term s PCustomValidator ->
  Term s (PMap any PDatumHash PDatum) ->
  Term s PScriptContext ->
  Term s PAddress ->
  Term s (PBuiltinPair (PAsData PTxOut) (PAsData PBool)) ->
  Term s PTxOut ->
  Term s PInteger
psmartHandleSuccessor validateFn datums ctx routeAddress smartInputRouteFlagPair routeOutput = P.do
  let smartInput = pfstBuiltin # smartInputRouteFlagPair
      forRoute = pfromData $ psndBuiltin # smartInputRouteFlagPair
  smartInputF <- pletFields @'["address", "value", "datum"] smartInput
  routeOutputF <- pletFields @'["address", "value", "datum"] routeOutput

  let smartInputDatum = pconvertChecked @PSmartHandleDatum $ presolveDatumData # smartInputF.datum # datums
      routeOutputDatum = presolveDatum # routeOutputF.datum # datums

  pif
    ( pmatch smartInputDatum $ \case
        PSimple ((pfield @"owner" #) -> owner) ->
          pif
            forRoute
            ( pand'List
                [ validateFn # pcon (PDJust $ pdcons # pdata owner # pdnil) # routerFeeForSimpleRoutes # smartInputF.value # punsafeCoerce (pconstant ()) # routeOutputDatum # pcon PTrue # ctx
                , ptraceIfFalse "Incorrect Route Output Value" (pvalueHasChangedByLovelaces # smartInputF.value # routeOutputF.value # negativeRouterFeeForSimpleRoutes)
                , ptraceIfFalse "Incorrect Route Address" (routeOutputF.address #== routeAddress)
                ]
            )
            (psignedByOwner # ctx # owner)
        PAdvanced dat' -> P.do
          let txInfo = pfield @"txInfo" # ctx
              mint = pfield @"mint" # txInfo
          datF <- pletFields @'["mOwner", "routerFee", "reclaimRouterFee", "routeRequiredMint", "reclaimRequiredMint", "extraInfo"] dat'
          pif
            forRoute
            ( let
                inputIncludingMint =
                  papplyRequiredMintToInputValue
                    mint
                    datF.routeRequiredMint
                    smartInputF.value
               in
                pand'List
                  [ validateFn # datF.mOwner # datF.routerFee # smartInputF.value # datF.extraInfo # routeOutputDatum # forRoute # ctx
                  , ptraceIfFalse "Incorrect Route Output Value" (pvalueHasChangedByLovelaces # inputIncludingMint # routeOutputF.value # (pnegate # datF.routerFee))
                  , ptraceIfFalse "Incorrect Route Address" (routeOutputF.address #== routeAddress)
                  ]
            )
            ( pmatch (pfield @"mOwner" # dat') $ \case
                PDJust ((pfield @"_0" #) -> owner) ->
                  let
                    inputIncludingMint =
                      papplyRequiredMintToInputValue
                        mint
                        datF.reclaimRequiredMint
                        smartInputF.value
                   in
                    pand'List
                      [ validateFn # datF.mOwner # datF.reclaimRouterFee # smartInputF.value # datF.extraInfo # routeOutputDatum # forRoute # ctx
                      , ptraceIfFalse "Incorrect Reclaim Output Value" (pvalueHasChangedByLovelaces # inputIncludingMint # routeOutputF.value # (pnegate # datF.reclaimRouterFee))
                      , ptraceIfFalse "Incorrect Reclaim Address" (routeOutputF.address #== owner)
                      ]
                PDNothing _ ->
                  perror
            )
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

smartHandleStakeValidatorW :: Term s (PCustomValidator :--> PAddress :--> PStakeValidator)
smartHandleStakeValidatorW = phoistAcyclic $ plam $ \validateFn routeAddress redeemer ctx -> P.do
  let red = pconvertUnsafe @PRouterRedeemer redeemer
  redF <- pletFields @'["inputIdxs", "outputIdxs"] red
  ctxF <- pletFields @'["txInfo", "purpose"] ctx
  PRewarding _ <- pmatch ctxF.purpose
  infoF <- pletFields @'["inputs", "outputs", "redeemers", "datums"] ctxF.txInfo
  txInputs <- plet infoF.inputs
  txOuts <- plet infoF.outputs

  -- let smartInputs = puniqueOrdered # plam (\idx -> pfield @"resolved" #$ pelemAt @PBuiltinList # idx # txInputs) # 0 # redF.inputIdxs
  let smartInputs = puniqueOrdered # plam (\idx -> pelemAt @PBuiltinList # idx # txInputs) # 0 # redF.inputIdxs
      routeOutputs = puniqueOrdered # plam (\idx -> pelemAt @PBuiltinList # idx # txOuts) # 0 # redF.outputIdxs
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
      inUTxORouteFlagPairs =
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
                      PRouteSmart _ ->
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
          routeAddress
          0
          inUTxORouteFlagPairs
          routeOutputs

  let scInpCount = pcountScriptInputs # txInputs
      foldChecks =
        pand'List
          [foldCount #== scInpCount] -- possibly add protocol fee payout
  pif foldChecks (popaque $ pconstant ()) perror
