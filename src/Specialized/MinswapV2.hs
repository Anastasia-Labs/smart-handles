module Specialized.MinswapV2 where

import PlutusLedgerApi.V1 (Address, BuiltinByteString, CurrencySymbol, PubKeyHash, ScriptHash, TokenName)
import PlutusTx qualified

import Plutarch.DataRepr
import Plutarch.Lift (PConstantDecl, PUnsafeLiftDecl (..))
import Plutarch.Monadic qualified as P
import Plutarch.Prelude

import Plutarch.Api.V1 (PAddress, PCurrencySymbol, PPubKeyHash, PTokenName)
import Plutarch.Api.V1.Address (PCredential (..))
import Plutarch.Api.V2 (PMaybeData (..), PScriptContext, PScriptHash, PStakeValidator)

import SingleValidator (PSmartHandleDatum, PSmartHandleRedeemer, psmartHandleValidator)
import StakingValidator (smartHandleStakeValidatorW)
import Utils (PCustomValidator, PTriple (..), pand'List, pconvertChecked, pgetSingleAsset)

import Specialized.MinswapV2.Constants (padaToMinLPTokenName, pbatcherFee, pminswapV2LPSymbol)
import Specialized.MinswapV2.Utils (plovelacesAfterFees)

--------------------------------------------------------------------------------

data BoolData
  = FalseData
  | TrueData

PlutusTx.makeLift ''BoolData
PlutusTx.makeIsDataIndexed
  ''BoolData
  [ ('FalseData, 0)
  , ('TrueData, 1)
  ]

data PBoolData (s :: S)
  = PFalseData (Term s (PDataRecord '[]))
  | PTrueData (Term s (PDataRecord '[]))
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PEq)

instance DerivePlutusType PBoolData where
  type DPTStrat _ = PlutusTypeData

instance PTryFrom PData PBoolData

instance PUnsafeLiftDecl PBoolData where type PLifted PBoolData = BoolData
deriving via (DerivePConstantViaData BoolData PBoolData) instance PConstantDecl BoolData

--------------------------------------------------------------------------------

data Asset = Asset
  { policyId :: CurrencySymbol
  , assetName :: TokenName
  }

PlutusTx.makeLift ''Asset
PlutusTx.makeIsDataIndexed ''Asset [('Asset, 0)]

data PAsset (s :: S)
  = PAsset
      ( Term
          s
          ( PDataRecord
              '[ "policyId" ':= PCurrencySymbol
               , "assetName" ':= PTokenName
               ]
          )
      )
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PDataFields)

instance DerivePlutusType PAsset where
  type DPTStrat _ = PlutusTypeData

instance PTryFrom PData PAsset

instance PUnsafeLiftDecl PAsset where type PLifted PAsset = Asset
deriving via (DerivePConstantViaData Asset PAsset) instance PConstantDecl Asset

--------------------------------------------------------------------------------

data SwapAmountOption
  = SAOSpecificAmount Integer
  | SAOAll Integer

PlutusTx.makeLift ''SwapAmountOption
PlutusTx.makeIsDataIndexed
  ''SwapAmountOption
  [ ('SAOSpecificAmount, 0)
  , ('SAOAll, 1)
  ]

data PSwapAmountOption (s :: S)
  = PSAOSpecificAmount (Term s (PDataRecord '["swapAmount" ':= PInteger]))
  | PSAOAll (Term s (PDataRecord '["deductedAmount" ':= PInteger]))
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PEq)

instance DerivePlutusType PSwapAmountOption where
  type DPTStrat _ = PlutusTypeData

instance PTryFrom PData PSwapAmountOption

instance PUnsafeLiftDecl PSwapAmountOption where type PLifted PSwapAmountOption = SwapAmountOption
deriving via (DerivePConstantViaData SwapAmountOption PSwapAmountOption) instance PConstantDecl SwapAmountOption

--------------------------------------------------------------------------------

data OrderStep
  = SwapExactIn Bool SwapAmountOption Integer Bool
  | SwapExactOut Bool SwapAmountOption Integer Bool

-- Omitting the other constructors for the time being.

PlutusTx.makeLift ''OrderStep
PlutusTx.makeIsDataIndexed
  ''OrderStep
  [ ('SwapExactIn, 0)
  , ('SwapExactOut, 1)
  ]

data POrderStep (s :: S)
  = PSwapExactIn
      ( Term
          s
          ( PDataRecord
              '[ "aToBDirection" ':= PBoolData
               , "swapAmountOption" ':= PSwapAmountOption
               , "minimumReceive" ':= PInteger
               , "killable" ':= PBoolData
               ]
          )
      )
  | PSwapExactOut
      ( Term
          s
          ( PDataRecord
              '[ "aToBDirection" ':= PBoolData
               , "maximumSwapAmountOption" ':= PSwapAmountOption
               , "expectedReceive" ':= PInteger
               , "killable" ':= PBoolData
               ]
          )
      )
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData)

instance DerivePlutusType POrderStep where
  type DPTStrat _ = PlutusTypeData

instance PTryFrom PData POrderStep

instance PUnsafeLiftDecl POrderStep where type PLifted POrderStep = OrderStep
deriving via (DerivePConstantViaData OrderStep POrderStep) instance PConstantDecl OrderStep

--------------------------------------------------------------------------------

data ExtraOrderDatum
  = EODNoDatum
  | EODDatumHash BuiltinByteString
  | EODInlineDatum BuiltinByteString

PlutusTx.makeLift ''ExtraOrderDatum
PlutusTx.makeIsDataIndexed
  ''ExtraOrderDatum
  [ ('EODNoDatum, 0)
  , ('EODDatumHash, 1)
  , ('EODInlineDatum, 2)
  ]

data PExtraOrderDatum (s :: S)
  = PEODNoDatum (Term s (PDataRecord '[]))
  | PEODDatumHash (Term s (PDataRecord '["hash" ':= PByteString]))
  | PEODInlineDatum (Term s (PDataRecord '["hash" ':= PByteString]))
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData)

instance DerivePlutusType PExtraOrderDatum where
  type DPTStrat _ = PlutusTypeData

instance PTryFrom PData PExtraOrderDatum

instance PUnsafeLiftDecl PExtraOrderDatum where type PLifted PExtraOrderDatum = ExtraOrderDatum
deriving via (DerivePConstantViaData ExtraOrderDatum PExtraOrderDatum) instance PConstantDecl ExtraOrderDatum

--------------------------------------------------------------------------------

data OrderAuthorizationMethod
  = OAMSignature PubKeyHash
  | OAMSpendScript ScriptHash
  | OAMWithdrawScript ScriptHash
  | OAMMintScript ScriptHash

PlutusTx.makeLift ''OrderAuthorizationMethod
PlutusTx.makeIsDataIndexed
  ''OrderAuthorizationMethod
  [ ('OAMSignature, 0)
  , ('OAMSpendScript, 1)
  , ('OAMWithdrawScript, 2)
  , ('OAMMintScript, 3)
  ]

data POrderAuthorizationMethod (s :: S)
  = POAMSignature (Term s (PDataRecord '["pubKeyHash" ':= PPubKeyHash]))
  | POAMSpendScript (Term s (PDataRecord '["scriptHash" ':= PScriptHash]))
  | POAMWithdrawScript (Term s (PDataRecord '["scriptHash" ':= PScriptHash]))
  | POAMMintScript (Term s (PDataRecord '["scriptHash" ':= PScriptHash]))
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData)

instance DerivePlutusType POrderAuthorizationMethod where
  type DPTStrat _ = PlutusTypeData

instance PTryFrom PData POrderAuthorizationMethod

instance PUnsafeLiftDecl POrderAuthorizationMethod where type PLifted POrderAuthorizationMethod = OrderAuthorizationMethod
deriving via (DerivePConstantViaData OrderAuthorizationMethod POrderAuthorizationMethod) instance PConstantDecl OrderAuthorizationMethod

--------------------------------------------------------------------------------

data OrderDatum = OrderDatum
  { canceller :: OrderAuthorizationMethod
  , refundReceiver :: Address
  , refundReceiverDatum :: ExtraOrderDatum
  , successReceiver :: Address
  , successReceiverDatum :: ExtraOrderDatum
  , lpAsset :: Asset
  , step :: OrderStep
  , maxBatcherFee :: Integer
  , expirySettingOpt :: Maybe (Integer, Integer)
  }

PlutusTx.makeLift ''OrderDatum
PlutusTx.makeIsDataIndexed ''OrderDatum [('OrderDatum, 0)]

data POrderDatum (s :: S)
  = POrderDatum
      ( Term
          s
          ( PDataRecord
              '[ "canceller" ':= POrderAuthorizationMethod
               , "refundReceiver" ':= PAddress
               , "refundReceiverDatum" ':= PExtraOrderDatum
               , "successReceiver" ':= PAddress
               , "successReceiverDatum" ':= PExtraOrderDatum
               , "lpAsset" ':= PAsset
               , "step" ':= POrderStep
               , "maxBatcherFee" ':= PInteger
               , "expirySettingOpt" ':= PMaybeData (PAsData (PBuiltinPair (PAsData PInteger) (PAsData PInteger)))
               ]
          )
      )
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PDataFields)

instance DerivePlutusType POrderDatum where
  type DPTStrat _ = PlutusTypeData

instance PTryFrom PData POrderDatum

instance PUnsafeLiftDecl POrderDatum where type PLifted POrderDatum = OrderDatum
deriving via (DerivePConstantViaData OrderDatum POrderDatum) instance PConstantDecl OrderDatum

--------------------------------------------------------------------------------

data MinswapRequest = MinswapRequest Bool Integer Bool

PlutusTx.makeLift ''MinswapRequest
PlutusTx.makeIsDataIndexed ''MinswapRequest [('MinswapRequest, 0)]

data PMinswapRequest (s :: S)
  = PMinswapRequest
      ( Term
          s
          ( PDataRecord
              '[ "aToBDirection" ':= PBoolData
               , "minimumReceive" ':= PInteger
               ]
          )
      )
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PDataFields)

instance DerivePlutusType PMinswapRequest where
  type DPTStrat _ = PlutusTypeData

instance PTryFrom PData PMinswapRequest

instance PUnsafeLiftDecl PMinswapRequest where type PLifted PMinswapRequest = MinswapRequest
deriving via (DerivePConstantViaData MinswapRequest PMinswapRequest) instance PConstantDecl MinswapRequest

--------------------------------------------------------------------------------

validateFn :: forall (s :: S). Term s PCustomValidator
validateFn = plam $ \mOwner _routingFee inputValue _extraInfoData outputDatum _forRoute _ctx -> P.do
  ---- INITIAL SETUP -----------------------------------------------------------
  PDJust ((pfield @"_0" #) -> owner) <- pmatch mOwner
  PPubKeyCredential ((pfield @"_0" #) -> ownerPKH) <- pmatch (pfield @"credential" # owner)
  let orderDatum = pconvertChecked @POrderDatum (pto outputDatum)
  orderDatumF <- pletFields @'["canceller", "refundReceiver", "refundReceiverDatum", "successReceiver", "successReceiverDatum", "lpAsset", "step", "maxBatcherFee", "expirySettingOpt"] orderDatum

  ---- REFUND VALIDATION -------------------------------------------------------
  POAMSignature ((pfield @"pubKeyHash" #) -> specifiedCancellerPKH) <- pmatch orderDatumF.canceller
  PEODNoDatum _ <- pmatch orderDatumF.refundReceiverDatum
  let validRefund =
        pand'List
          [ ptraceIfFalse "Refund receiver must be the owner" (orderDatumF.refundReceiver #== owner)
          , ptraceIfFalse "Specified canceller must be the owner" (specifiedCancellerPKH #== ownerPKH)
          ]

  ---- SUCCESS VALIDATION ------------------------------------------------------
  PEODNoDatum _ <- pmatch orderDatumF.successReceiverDatum
  let validSuccess = ptraceIfFalse "Invalid success receiver" (orderDatumF.successReceiver #== owner)

  ---- SWAP VALIDATION ---------------------------------------------------------
  lpAssetF <- pletFields @'["policyId", "assetName"] orderDatumF.lpAsset
  PSwapExactIn pseFields <- pmatch orderDatumF.step
  stepF <- pletFields @'["aToBDirection", "swapAmountOption", "minimumReceive", "killable"] pseFields
  PTrueData _ <- pmatch stepF.aToBDirection
  let inputQuantity =
        pmatch (pgetSingleAsset inputValue) $ \(PTriple _ _ inLovelaces) ->
          plovelacesAfterFees # inLovelaces

      validSwap =
        pand'List
          [ lpAssetF.policyId #== pminswapV2LPSymbol
          , lpAssetF.assetName #== padaToMinLPTokenName
          , stepF.swapAmountOption #== pcon (PSAOSpecificAmount $ pdcons # pdata inputQuantity # pdnil)
          , stepF.killable #== pcon (PTrueData pdnil)
          ]

  ---- EXTRA VALIDATIONS -------------------------------------------------------
  let extraValidations =
        pand'List
          [ orderDatumF.maxBatcherFee #== pbatcherFee
          , orderDatumF.expirySettingOpt #== pcon (PDNothing pdnil)
          ]

  pand'List
    [ ptraceIfFalse "Invalid refund info" validRefund
    , ptraceIfFalse "Invalid success info" validSuccess
    , ptraceIfFalse "Invalid swap" validSwap
    , extraValidations
    ]

psingleValidator :: Term s (PAddress :--> PSmartHandleDatum :--> PSmartHandleRedeemer :--> PScriptContext :--> PUnit)
psingleValidator = psmartHandleValidator # validateFn

pstakeValidator :: Term s (PAddress :--> PStakeValidator)
pstakeValidator = smartHandleStakeValidatorW # validateFn
