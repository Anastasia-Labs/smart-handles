module SingleValidator where

import PlutusLedgerApi.V2 (Address)
import PlutusTx qualified

import Plutarch.Api.V1.Address (PCredential (..))
import Plutarch.Api.V2 (PAddress, PMaybeData (..), PScriptContext, PScriptHash, PScriptPurpose (..), PTxInInfo, PTxOut, PTxOutRef, PValidator)
import Plutarch.DataRepr
import Plutarch.Lift (PConstantDecl, PUnsafeLiftDecl (..))
import Plutarch.Monadic qualified as P
import Plutarch.Num (PNum (pnegate))
import Plutarch.Prelude
import Plutarch.Unsafe (punsafeCoerce)
import "liqwid-plutarch-extra" Plutarch.Extra.ScriptContext ()

import Constants (routerFeeAsNegativeLovelace)
import Utils (PCustomValidator, pand'List, pconvertChecked, pconvertUnsafe, presolveDatum, psignedByOwner, pvalueHasChangedByLovelaces)

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

data SmartHandleDatum
  = Simple Address -- <-- owner
  | Advanced (Maybe Address) Integer Integer PlutusTx.BuiltinData

--           ^-------------^ ^-----^ ^-----^ ^------------------^
--               mOwner    routerFee reclaimRouterFee   extraInfo

PlutusTx.makeLift ''SmartHandleDatum
PlutusTx.makeIsDataIndexed
  ''SmartHandleDatum
  [ ('Simple, 0)
  , ('Advanced, 1)
  ]

data PSmartHandleDatum (s :: S)
  = PSimple
      ( Term
          s
          ( PDataRecord
              '[ "owner" ':= PAddress
               ]
          )
      )
  | PAdvanced
      ( Term
          s
          ( PDataRecord
              '[ "mOwner" ':= PMaybeData PAddress
               , "routerFee" ':= PInteger
               , "reclaimRouterFee" ':= PInteger
               , "extraInfo" ':= PData
               ]
          )
      )
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData)

instance DerivePlutusType PSmartHandleDatum where
  type DPTStrat _ = PlutusTypeData

instance PTryFrom PData PSmartHandleDatum

instance PUnsafeLiftDecl PSmartHandleDatum where type PLifted PSmartHandleDatum = SmartHandleDatum
deriving via (DerivePConstantViaData SmartHandleDatum PSmartHandleDatum) instance PConstantDecl SmartHandleDatum

data SmartHandleRedeemer
  = Route
      { ownIndex :: Integer
      , routerIndex :: Integer
      }
  | Reclaim
  | AdvancedReclaim
      { ownIndex :: Integer
      , routerIndex :: Integer
      }

PlutusTx.makeLift ''SmartHandleRedeemer
PlutusTx.makeIsDataIndexed
  ''SmartHandleRedeemer
  [ ('Route, 0)
  , ('Reclaim, 1)
  , ('AdvancedReclaim, 2)
  ]

data PSmartHandleRedeemer (s :: S)
  = PRoute (Term s (PDataRecord '["ownIndex" ':= PInteger, "routerIndex" ':= PInteger]))
  | PReclaim (Term s (PDataRecord '[]))
  | PAdvancedReclaim (Term s (PDataRecord '["ownIndex" ':= PInteger, "routerIndex" ':= PInteger]))
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData)

instance DerivePlutusType PSmartHandleRedeemer where
  type DPTStrat _ = PlutusTypeData

instance PTryFrom PData PSmartHandleRedeemer

instance PUnsafeLiftDecl PSmartHandleRedeemer where type PLifted PSmartHandleRedeemer = SmartHandleRedeemer
deriving via (DerivePConstantViaData SmartHandleRedeemer PSmartHandleRedeemer) instance PConstantDecl SmartHandleRedeemer

data PSmartMetadataDatum (s :: S)
  = PSmartMetadataDatum (Term s (PDataRecord '["metadata" ':= PData, "version" ':= PInteger, "extra" ':= PSmartRouter]))
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PDataFields, PEq)

instance DerivePlutusType PSmartMetadataDatum where
  type DPTStrat _ = PlutusTypeData

instance PTryFrom PData PSmartMetadataDatum

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

psmartHandleValidatorW :: Term s (PCustomValidator :--> PAddress :--> PValidator)
psmartHandleValidatorW = phoistAcyclic $ plam $ \validateFn routeAddress dat red ctx ->
  let datum = pconvertChecked @PSmartHandleDatum dat
      redeemer = pconvertUnsafe @PSmartHandleRedeemer red
   in popaque $ psmartHandleValidator # validateFn # routeAddress # datum # redeemer # ctx

psmartHandleValidator :: Term s (PCustomValidator :--> PAddress :--> PSmartHandleDatum :--> PSmartHandleRedeemer :--> PScriptContext :--> PUnit)
psmartHandleValidator = phoistAcyclic $ plam $ \validateFn routeAddress dat red ctx -> pmatch red $ \case
  PRoute r ->
    pmatch dat $ \case
      PSimple _ ->
        pletFields @'["ownIndex", "routerIndex"] r $ \redF -> P.do
          prouter # validateFn # routeAddress # dat # redF.ownIndex # redF.routerIndex # pcon PTrue # ctx
      PAdvanced _ -> P.do
        pletFields @'["ownIndex", "routerIndex", "advancedRedeemer"] r $ \redF -> P.do
          prouter # validateFn # routeAddress # dat # redF.ownIndex # redF.routerIndex # pcon PTrue # ctx
  PReclaim _ ->
    pmatch dat $ \case
      PSimple ((pfield @"owner" #) -> owner) ->
        psignedByOwner # ctx # owner
      PAdvanced _ ->
        perror
  PAdvancedReclaim r ->
    pmatch dat $ \case
      PSimple _ ->
        perror
      PAdvanced dat' ->
        pletFields @'["ownIndex", "routerIndex"] r $ \redF ->
          pletFields @'["mOwner", "routerFee", "extraInfo"] dat' $ \datF -> P.do
            pmatch datF.mOwner $ \case
              PDJust ((pfield @"_0" #) -> owner) ->
                prouter # validateFn # owner # dat # redF.ownIndex # redF.routerIndex # pcon PFalse # ctx
              PDNothing _ ->
                perror

prouter ::
  Term
    s
    ( PCustomValidator
        :--> PAddress
        :--> PSmartHandleDatum
        :--> PInteger
        :--> PInteger
        :--> PBool
        :--> PScriptContext
        :--> PUnit
    )
prouter = phoistAcyclic $ plam $ \validateFn routeAddress dat ownIndex routerIndex forRoute ctx -> P.do
  ctxF <- pletFields @'["txInfo", "purpose"] ctx
  infoF <- pletFields @'["inputs", "outputs", "signatories", "datums"] ctxF.txInfo
  PSpending ((pfield @"_0" #) -> ownRef) <- pmatch ctxF.purpose
  indexedInput <- pletFields @'["outRef", "resolved"] (pelemAt @PBuiltinList # ownIndex # infoF.inputs)

  ownInputF <- pletFields @'["value", "address"] indexedInput.resolved
  PScriptCredential ((pfield @"_0" #) -> ownValHash) <- pmatch (pfield @"credential" # ownInputF.address)

  routerOutputF <- pletFields @'["datum", "value", "address"] (pelemAt @PBuiltinList # routerIndex # infoF.outputs)

  let outputDatum = presolveDatum # routerOutputF.datum # infoF.datums
  pif
    ( pand'List
        [ ptraceIfFalse "Incorrect Indexed Input" (ownRef #== indexedInput.outRef)
        , ptraceIfFalse "Incorrect Route Address" (routerOutputF.address #== routeAddress)
        , ptraceIfFalse "Multiple Script Inputs Spent" (pcountInputsAtScript # ownValHash # infoF.inputs #== 1)
        , pmatch dat $ \case
            PSimple ((pfield @"owner" #) -> owner) ->
              pand'List
                [ validateFn # pcon (PDJust $ pdcons # pdata owner # pdnil) # punsafeCoerce (pconstant ()) # routerOutputF.value # outputDatum # forRoute # ctx
                , ptraceIfFalse "Incorrect Route Output Value" (pvalueHasChangedByLovelaces # ownInputF.value # routerOutputF.value # routerFeeAsNegativeLovelace)
                ]
            PAdvanced dat' -> P.do
              datF <- pletFields @'["mOwner", "routerFee", "reclaimRouterFee", "extraInfo"] dat'
              let routerFee = pif forRoute (pnegate # datF.routerFee) (pnegate # datF.reclaimRouterFee)
              pand'List
                [ validateFn # datF.mOwner # datF.extraInfo # routerOutputF.value # outputDatum # forRoute # ctx
                , ptraceIfFalse
                    "Incorrect Route Output Value"
                    (pvalueHasChangedByLovelaces # ownInputF.value # routerOutputF.value # routerFee)
                ]
        ]
    )
    (pconstant ())
    perror
