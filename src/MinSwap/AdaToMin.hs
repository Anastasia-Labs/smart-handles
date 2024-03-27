module MinSwap.AdaToMin where

import PlutusLedgerApi.V1.Value (AssetClass)
import PlutusLedgerApi.V2 (Address (..), Credential (..), CurrencySymbol, DatumHash, PubKeyHash, StakingCredential (..), TokenName)
import PlutusTx qualified

import Plutarch.Api.V2 (PAddress, PCurrencySymbol, PDatum, PDatumHash, PMaybeData (..), PPubKeyHash, PScriptContext, PStakeValidator, PTokenName)
import Plutarch.DataRepr
import Plutarch.Lift (PConstantDecl, PUnsafeLiftDecl (..))
import Plutarch.Monadic qualified as P
import Plutarch.Prelude
import "liqwid-plutarch-extra" Plutarch.Extra.ScriptContext ()

import SingleValidator (PSmartHandleDatum, PSmartHandleRedeemer, psmartHandleValidator)
import StakingValidator (smartHandleStakeValidatorW)
import Utils

-- Smart Beacon @adaToMin
-- user sends 50 ADA to @adaToMin
-- 49 ADA is swapped for Min Token and user receives the result
-- routing agent is able to take 1 ADA fee.
data OrderType = OrderType
  { desiredAsset :: AssetClass
  , minReceive :: Integer
  }

-- TODO: Extend OrderType to facilitate below
--  | OrderTypeOut
--     { desiredAsset :: AssetClass
--     , expectedReceived :: Integer
--     }
--  | OrderTypeDeposit
--     { minimumLP :: Integer
--     }
--  | OrderTypeWithdraw
--     { minimumAssetA :: Integer
--     , minimumAssetAB :: Integer
--     }

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

adaToMinTN :: Term s PTokenName
adaToMinTN =
  let tn :: TokenName
      tn = "$adaToMin"
   in pconstant tn

-- $MIN Currency Symbol on Preprod
minCS :: Term s PCurrencySymbol
minCS =
  let cs :: CurrencySymbol
      cs = "e16c2dc8ae937e8d3790c7fd7168d7b994621ba14ca11415f39fed72"
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

validateFn :: Term s (PAddress :--> PCurrencySymbol :--> PTokenName :--> PDatum :--> PBool)
validateFn = plam $ \owner desiredAssetSymbol desiredAssetTokenName outputDatum -> P.do
  let outDatum = pconvertChecked @PMinswapRequestDatum (pto outputDatum)
  outDatumF <- pletFields @'["sender", "receiver", "receiverDatumHash", "step", "batcherFee", "outputAda"] outDatum
  orderStepF <- pletFields @'["desiredAsset", "minReceive"] outDatumF.step
  desiredAssetF <- pletFields @'["cs", "tn"] orderStepF.desiredAsset
  pand'List
    [ ptraceIfFalse "Incorrect Swap Sender" (outDatumF.sender #== owner)
    , ptraceIfFalse "Incorrect Swap Receiver" (outDatumF.receiver #== owner)
    , ptraceIfFalse
        "Incorrect ReceiverDatumHash"
        ( pmatch outDatumF.receiverDatumHash $ \case
            PDJust _ -> pconstant False
            PDNothing _ -> pconstant True
        )
    , ptraceIfFalse "Incorrect $MIN Policy Id" (desiredAssetF.cs #== desiredAssetSymbol)
    , ptraceIfFalse "Incorrect $MIN Token Name" (desiredAssetF.tn #== desiredAssetTokenName)
    , ptraceIfFalse "Incorrect Batcher Fee" (pfromData outDatumF.batcherFee #== pconstant 2_000_000)
    , ptraceIfFalse "Incorrect Output ADA" (pfromData outDatumF.outputAda #== pconstant 2_000_000)
    ]

psingleValidator :: Term s (PAddress :--> PSmartHandleDatum :--> PSmartHandleRedeemer :--> PScriptContext :--> PUnit)
psingleValidator = psmartHandleValidator # validateFn

pstakeValidator :: Term s (PAddress :--> PStakeValidator)
pstakeValidator = smartHandleStakeValidatorW # validateFn
