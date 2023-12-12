{-# OPTIONS_GHC -Wno-unused-imports #-}

module SmartHandles where

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

import Conversions
import Utils

-- Smart Beacon @adaToMin
-- user sends 50 ADA to @adaToMin
-- 49 ADA is swapped for Min Token and user receives the result
-- routing agent is able to take 1 ADA fee.

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

pcountInputsAtScript :: Term s (PScriptHash :--> PBuiltinList PTxInInfo :--> PInteger)
pcountInputsAtScript =
  phoistAcyclic $ plam $ \sHash ->
    let go :: Term _ (PInteger :--> PBuiltinList PTxInInfo :--> PInteger)
        go = pfix #$ plam $ \self n ->
          pelimList
            ( \x xs ->
                let cred = pfield @"credential" # (pfield @"address" # (pfield @"resolved" # x))
                 in pmatch cred $ \case
                      PScriptCredential ((pfield @"_0" #) -> vh) -> pif (sHash #== vh) (self # (n + 1) # xs) (self # n # xs)
                      _ -> self # n # xs
            )
            n
     in go # 0

data PAssetClass (s :: S) = PAssetClass (Term s (PDataRecord '["cs" ':= PCurrencySymbol, "tn" ':= PTokenName]))
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PDataFields)

instance DerivePlutusType PAssetClass where
  type DPTStrat _ = PlutusTypeData

instance PTryFrom PData PAssetClass

data SmartHandleDatum = SmartHandleDatum
  { owner :: Address
  }

PlutusTx.makeLift ''SmartHandleDatum
PlutusTx.makeIsDataIndexed ''SmartHandleDatum [('SmartHandleDatum, 0)]

data PSmartHandleDatum (s :: S) = PSmartHandleDatum (Term s (PDataRecord '["owner" ':= PAddress]))
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PDataFields)

instance DerivePlutusType PSmartHandleDatum where
  type DPTStrat _ = PlutusTypeData

instance PTryFrom PData PSmartHandleDatum

instance PUnsafeLiftDecl PSmartHandleDatum where type PLifted PSmartHandleDatum = SmartHandleDatum
deriving via (DerivePConstantViaData SmartHandleDatum PSmartHandleDatum) instance PConstantDecl SmartHandleDatum

data SmartHandleRedeemerSwap = SmartHandleRedeemerSwap
  { ownIndex :: Integer
  , routerIndex :: Integer
  }

PlutusTx.makeLift ''SmartHandleRedeemerSwap
PlutusTx.makeIsDataIndexed ''SmartHandleRedeemerSwap [('SmartHandleRedeemerSwap, 0)]

data SmartHandleRedeemer
  = Swap SmartHandleRedeemerSwap
  | Reclaim

PlutusTx.makeLift ''SmartHandleRedeemer
PlutusTx.makeIsDataIndexed
  ''SmartHandleRedeemer
  [ ('Swap, 0)
  , ('Reclaim, 1)
  ]

data PSmartHandleRedeemer (s :: S)
  = PSwap (Term s (PDataRecord '["ownIndex" ':= PInteger, "routerIndex" ':= PInteger]))
  | PReclaim (Term s (PDataRecord '[]))
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData)

instance DerivePlutusType PSmartHandleRedeemer where
  type DPTStrat _ = PlutusTypeData

instance PTryFrom PData PSmartHandleRedeemer

instance PUnsafeLiftDecl PSmartHandleRedeemer where type PLifted PSmartHandleRedeemer = SmartHandleRedeemer
deriving via (DerivePConstantViaData SmartHandleRedeemer PSmartHandleRedeemer) instance PConstantDecl SmartHandleRedeemer

data OrderType = OrderType
  { desiredAsset :: AssetClass
  , minReceive :: Integer
  }

PlutusTx.makeLift ''OrderType
PlutusTx.makeIsDataIndexed ''OrderType [('OrderType, 0)]

data POrderType (s :: S)
  = POrderType
      ( Term
          s
          ( PDataRecord
              '[ "desiredAsset" ':= PAssetClass
               , "minReceive" ':= PInteger
               ]
          )
      )
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PDataFields)

instance DerivePlutusType POrderType where
  type DPTStrat _ = PlutusTypeData

instance PTryFrom PData POrderType

instance PUnsafeLiftDecl POrderType where type PLifted POrderType = OrderType
deriving via (DerivePConstantViaData OrderType POrderType) instance PConstantDecl OrderType

data MinswapRequestDatum = MinswapRequestDatum
  { sender :: Address
  , receiver :: Address
  , receiverDatumHash :: Maybe DatumHash
  , step :: OrderType
  , batcherFee :: Integer
  , outputAda :: Integer
  }

PlutusTx.makeLift ''MinswapRequestDatum
PlutusTx.makeIsDataIndexed ''MinswapRequestDatum [('MinswapRequestDatum, 0)]

data PMinswapRequestDatum (s :: S)
  = PMinswapRequestDatum
      ( Term
          s
          ( PDataRecord
              '[ "sender" ':= PAddress
               , "receiver" ':= PAddress
               , "receiverDatumHash" ':= PMaybeData (PAsData PDatumHash)
               , "step" ':= POrderType
               , "batcherFee" ':= PInteger
               , "outputAda" ':= PInteger
               ]
          )
      )
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PDataFields)

instance DerivePlutusType PMinswapRequestDatum where
  type DPTStrat _ = PlutusTypeData

instance PTryFrom PData PMinswapRequestDatum

instance PUnsafeLiftDecl PMinswapRequestDatum where type PLifted PMinswapRequestDatum = MinswapRequestDatum
deriving via (DerivePConstantViaData MinswapRequestDatum PMinswapRequestDatum) instance PConstantDecl MinswapRequestDatum

data PSmartMetadataDatum (s :: S)
  = PSmartMetadataDatum (Term s (PDataRecord '["metadata" ':= PData, "version" ':= PInteger, "extra" ':= PSmartRouter]))
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PDataFields, PEq)

instance DerivePlutusType PSmartMetadataDatum where
  type DPTStrat _ = PlutusTypeData

instance PTryFrom PData PSmartMetadataDatum

adaToMinTN :: Term s PTokenName
adaToMinTN =
  let tn :: TokenName
      tn = "$adaToMin"
   in pconstant tn

minCS :: Term s PCurrencySymbol
minCS =
  let cs :: CurrencySymbol
      cs = "29d222ce763455e3d7a09a665ce554f00ac89d2e99a1a83d267170c6"
   in pconstant cs

minTN :: Term s PTokenName
minTN =
  let tn :: TokenName
      tn = "MIN"
   in pconstant tn

swapPkh :: Term s PPubKeyHash
swapPkh =
  let orderCred :: PubKeyHash
      orderCred = "a65ca58a4e9c755fa830173d2a5caed458ac0c73f97db7faae2e7e3b"
   in pconstant orderCred

minSwapAddress :: Term s PAddress
minSwapAddress =
  let orderCred = "a65ca58a4e9c755fa830173d2a5caed458ac0c73f97db7faae2e7e3b"
      orderStakeCred = PubKeyCredential "52563c5410bff6a0d43ccebb7c37e1f69f5eb260552521adff33b9c2"
      orderAddr = Address (ScriptCredential orderCred) (Just (StakingHash orderStakeCred))
   in pconstant orderAddr

-- data OrderDatum = OrderDatum
--   { odSender :: Address,                     => Owner
--     odReceiver :: Address,                   => Owner
--     odReceiverDatumHash :: Maybe DatumHash,  => Constr 1 []
--     odStep :: OrderStep,                     => Constr 0 [Constr 0 [policyId, tokenName], minAmount]
--     odBatcherFee :: Integer,                 => 2_000_000
--     odOutputADA :: Integer                   => 2_000_000
--   }
data PSmartRouter (s :: S)
  = PSmartRouter
      ( Term
          s
          ( PDataRecord
              -- 2 ADA Fee paid for the service of off-chain Laminar batcher to process transactions.
              '[ "batcherFee" ':= PInteger
               , -- 2 ADA This amount of ADA will be held as minimum UTxO ADA and will be returned when
                 -- your order is processed or cancelled.
                 "deposit" ':= PInteger
               ]
          )
      )
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PDataFields)

instance DerivePlutusType PSmartRouter where
  type DPTStrat _ = PlutusTypeData

instance PTryFrom PData PSmartRouter

ptryOwnInput :: (PIsListLike list PTxInInfo) => Term s (list PTxInInfo :--> PTxOutRef :--> PTxOut)
ptryOwnInput = phoistAcyclic $
  plam $ \inputs ownRef ->
    precList
      ( \self x xs ->
          pletFields @'["outRef", "resolved"] x $ \txInFields ->
            pif (ownRef #== txInFields.outRef) txInFields.resolved (self # xs)
      )
      (const perror)
      # inputs

psmartHandleValidatorW :: Term s (PAddress :--> PValidator)
psmartHandleValidatorW = phoistAcyclic $ plam $ \swapAddress dat red ctx ->
  let datum = pconvert dat
      redeemer = pconvert red
   in popaque $ psmartHandleValidator # swapAddress # datum # redeemer # ctx

psmartHandleValidator :: Term s (PAddress :--> PSmartHandleDatum :--> PSmartHandleRedeemer :--> PScriptContext :--> PUnit)
psmartHandleValidator = phoistAcyclic $ plam $ \swapAddress dat red ctx -> pmatch red $ \case
  PSwap r ->
    pletFields @'["ownIndex", "routerIndex"] r $ \redF ->
      pswapRouter # swapAddress # dat # redF.ownIndex # redF.routerIndex # ctx
  PReclaim _ ->
    pmatch (pfield @"credential" # (pfield @"owner" # dat)) $ \case
      PPubKeyCredential ((pfield @"_0" #) -> pkh) ->
        ( pif
            (pelem @PBuiltinList # pkh # (pfield @"signatories" # (pfield @"txInfo" # ctx)))
            (pconstant ())
            perror
        )
      _ -> perror

pswapRouter :: Term s (PAddress :--> PSmartHandleDatum :--> PInteger :--> PInteger :--> PScriptContext :--> PUnit)
pswapRouter = phoistAcyclic $ plam $ \swapAddress dat ownIndex routerIndex ctx -> P.do
  oldDatumF <- pletFields @'["owner"] dat
  ctxF <- pletFields @'["txInfo", "purpose"] ctx
  infoF <- pletFields @'["inputs", "outputs", "signatories", "datums"] ctxF.txInfo
  PSpending ((pfield @"_0" #) -> ownRef) <- pmatch ctxF.purpose
  indexedInput <- pletFields @'["outRef", "resolved"] (pelemAt @PBuiltinList # ownIndex # infoF.inputs)

  ownInputF <- pletFields @'["value", "address"] indexedInput.resolved
  PScriptCredential ((pfield @"_0" #) -> ownValHash) <- pmatch (pfield @"credential" # ownInputF.address)

  swapOutputF <- pletFields @'["datum", "value", "address"] (pelemAt @PBuiltinList # routerIndex # infoF.outputs)

  POutputDatumHash ((pfield @"datumHash" #) -> hash) <- pmatch swapOutputF.datum
  PJust outputDatum <- pmatch $ AssocMap.plookup # pfromData hash # infoF.datums
  let outDatum = ptryFrom @PMinswapRequestDatum (pto outputDatum) fst
  outDatumF <- pletFields @'["sender", "receiver", "receiverDatumHash", "step", "batcherFee", "outputAda"] outDatum
  orderStepF <- pletFields @'["desiredAsset", "minReceive"] outDatumF.step
  desiredAssetF <- pletFields @'["cs", "tn"] orderStepF.desiredAsset

  let routerFee = Value.psingleton # padaSymbol # padaToken # (-1_000_000)
  pif
    ( pand'List
        [ ownRef #== indexedInput.outRef
        , outDatumF.sender #== oldDatumF.owner
        , outDatumF.receiver #== oldDatumF.owner
        , pmatch outDatumF.receiverDatumHash $ \case
            PDJust _ -> pconstant False
            PDNothing _ -> pconstant True
        , desiredAssetF.cs #== minCS
        , desiredAssetF.tn #== minTN
        , pfromData outDatumF.batcherFee #== pconstant 2_000_000
        , pfromData outDatumF.outputAda #== pconstant 2_000_000
        , swapOutputF.address #== swapAddress
        , pforgetPositive swapOutputF.value #== (pforgetPositive ownInputF.value <> routerFee)
        , pcountInputsAtScript # ownValHash # infoF.inputs #== 1
        ]
    )
    (pconstant ())
    perror

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

pfoldl2 ::
  (PListLike listA, PListLike listB, PElemConstraint listA a, PElemConstraint listB b) =>
  Term s ((acc :--> a :--> b :--> acc) :--> acc :--> listA a :--> listB b :--> acc)
pfoldl2 =
  phoistAcyclic $ plam $ \func ->
    pfix #$ plam $ \self acc la lb ->
      pelimList
        ( \a as ->
            pelimList
              (\b bs -> self # (func # acc # a # b) # as # bs)
              perror
              lb
        )
        (pif (pnull # lb) acc perror)
        la

pfoldCorrespondingUTxOs ::
  Term s (PMap any PDatumHash PDatum) ->
  Term s PAddress ->
  Term s PInteger ->
  Term s (PBuiltinList PTxOut) ->
  Term s (PBuiltinList PTxOut) ->
  Term s PInteger
pfoldCorrespondingUTxOs datMap swapAddress acc la lb =
  pfoldl2
    # plam
      ( \state utxoIn utxoOut ->
          psmartHandleSuccessor datMap swapAddress state utxoIn utxoOut
      )
    # acc
    # la
    # lb

psmartHandleSuccessor ::
  Term s (PMap any PDatumHash PDatum) ->
  Term s PAddress ->
  Term s PInteger ->
  Term s PTxOut ->
  Term s PTxOut ->
  Term s PInteger
psmartHandleSuccessor datums swapAddress _foldCount smartInput swapOutput = P.do
  smartInputF <- pletFields @'["address", "value", "datum"] smartInput
  swapOutputF <- pletFields @'["address", "value", "datum"] swapOutput

  let smartInputDatum = pconvert $ presolveDatumData # smartInputF.datum # datums
  smartUser <- plet smartInputDatum

  POutputDatumHash ((pfield @"datumHash" #) -> hash) <- pmatch swapOutputF.datum
  PJust swapOutputDatum <- pmatch $ AssocMap.plookup # hash # datums
  let swapOutDatum = ptryFrom @PMinswapRequestDatum (pto swapOutputDatum) fst
  swapOutDatF <- pletFields @'["sender", "receiver", "receiverDatumHash", "step", "batcherFee", "outputAda"] swapOutDatum
  orderStepF <- pletFields @'["desiredAsset", "minReceive"] swapOutDatF.step
  desiredAssetF <- pletFields @'["cs", "tn"] orderStepF.desiredAsset

  pif
    ( pand'List
        [ plovelaceValueOf # smartInputF.value - 1_000_000 #== plovelaceValueOf # swapOutputF.value
        , swapOutDatF.sender #== smartUser
        , swapOutDatF.receiver #== smartUser
        , pmatch swapOutDatF.receiverDatumHash $ \case
            PDJust _ -> pconstant False
            PDNothing _ -> pconstant True
        , desiredAssetF.cs #== minCS
        , desiredAssetF.tn #== minTN
        , pfromData swapOutDatF.batcherFee #== pconstant 2_000_000
        , pfromData swapOutDatF.outputAda #== pconstant 2_000_000
        , swapOutputF.address #== swapAddress
        -- TODO: add some slippage check
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

smartHandleStakeValidatorW :: Term s (PAddress :--> PStakeValidator)
smartHandleStakeValidatorW = phoistAcyclic $ plam $ \swapAddress redeemer ctx -> P.do
  let red = pconvert @PRouterRedeemer redeemer
  redF <- pletFields @'["inputIdxs", "outputIdxs"] red
  ctxF <- pletFields @'["txInfo", "purpose"] ctx
  infoF <- pletFields @'["inputs", "outputs", "signatories", "datums"] ctxF.txInfo
  txInputs <- plet infoF.inputs
  txOuts <- plet infoF.outputs

  let smartInputs = puniqueOrdered # plam (\idx -> pfield @"resolved" #$ pelemAt @PBuiltinList # idx # txInputs) # 0 # redF.inputIdxs
      swapOutputs = puniqueOrdered # plam (\idx -> pelemAt @PBuiltinList # idx # txOuts) # 0 # redF.outputIdxs
      foldCount = pfoldCorrespondingUTxOs infoF.datums swapAddress 0 smartInputs swapOutputs

  let scInpCount = pcountScriptInputs # txInputs
      foldChecks =
        pand'List
          [foldCount #== scInpCount] -- possibly add protocol fee payout
  pif foldChecks (popaque $ pconstant ()) perror

data SmartRedeemer
  = SwapSmart
  | ReclaimSmart

PlutusTx.makeLift ''SmartRedeemer
PlutusTx.makeIsDataIndexed
  ''SmartRedeemer
  [ ('SwapSmart, 0)
  , ('ReclaimSmart, 1)
  ]

data PSmartRedeemer (s :: S)
  = PSwapSmart (Term s (PDataRecord '[]))
  | PReclaimSmart (Term s (PDataRecord '[]))
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData)

instance DerivePlutusType PSmartRedeemer where
  type DPTStrat _ = PlutusTypeData

instance PTryFrom PData PSmartRedeemer

instance PUnsafeLiftDecl PSmartRedeemer where type PLifted PSmartRedeemer = SmartRedeemer
deriving via (DerivePConstantViaData SmartRedeemer PSmartRedeemer) instance PConstantDecl SmartRedeemer

smartHandleRouteValidatorW :: Term s (PStakingCredential :--> PValidator)
smartHandleRouteValidatorW = phoistAcyclic $ plam $ \stakeScript datum redeemer ctx -> P.do
  let red = pconvert @PSmartRedeemer redeemer
      dat = pconvert @PAddress datum
  ctxF <- pletFields @'["txInfo"] ctx
  pmatch red $ \case
    PSwapSmart _ ->
      let stakeCerts = pfield @"wdrl" # ctxF.txInfo
       in pmatch (AssocMap.plookup # stakeScript # stakeCerts) $ \case
            PJust _ -> (popaque $ pconstant ())
            PNothing -> perror
    PReclaimSmart _ ->
      pmatch (pfield @"credential" # dat) $ \case
        PPubKeyCredential ((pfield @"_0" #) -> pkh) ->
          ( pif
              (pelem @PBuiltinList # pkh # (pfield @"signatories" # ctxF.txInfo))
              (popaque $ pconstant ())
              perror
          )
        PScriptCredential _ -> perror -- TODO: is it refundable for a script?
