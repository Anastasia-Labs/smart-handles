module SmartHandlesSpec (tests) where

import Data.Either (fromRight)

import PlutusLedgerApi.V1.Value (AssetClass (..), assetClass)
import PlutusLedgerApi.V2 (Address (..), Credential (..), ScriptContext, ScriptPurpose (Rewarding, Spending), StakingCredential (..), TxId (TxId), TxOutRef (TxOutRef), adaSymbol, adaToken, singleton)
import PlutusTx (toBuiltinData, toData)

import Plutarch
import Plutarch.Api.V2 (scriptHash)
import Plutarch.Context (Builder, address, buildRewarding', extraRedeemer, input, output, script, withDatum, withRedeemer, withRef, withValue, withdrawal)
import Plutarch.Prelude
import Plutarch.Test.Precompiled (tryFromPTerm, (@!>), (@>))
import Plutarch.Test.QuickCheck (TestableTerm (..), fromFailingPPartial, fromPFun)

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (Gen, Property, chooseInt, chooseInteger, forAll, shuffle, sublistOf, testProperty)

import BatchValidator (SmartRedeemer (..), smartHandleRouteValidatorW)
import Compilation
import Debug.Trace (trace)
import SingleValidator (ReclaimMint (..), SmartHandleDatum (..))
import Specialized.Minswap
import StakingValidator (RouterRedeemer (..), puniqueOrdered)

tests :: TestTree
tests = testGroup "Smart handles" [uniqueOrderedTests, stakingValidatorTests]

uniqueOrderedTests :: TestTree
uniqueOrderedTests =
  testGroup
    "puniqueOrdered"
    [ testProperty "succeeds on no duplicates" property_puniqueOrdered_successOnNoDuplicates
    , testProperty "fails on any duplicates" property_puniqueOrdered_errorOnDuplicates
    ]

property_puniqueOrdered_successOnNoDuplicates :: Property
property_puniqueOrdered_successOnNoDuplicates = forAll indexList $ fromPFun $ check
  where
    indexList :: Gen (TestableTerm (PBuiltinList (PAsData PInteger)))
    indexList = do
      n <- chooseInteger (1, 100)
      s <- sublistOf [1 .. n]
      l <- shuffle s
      return $ TestableTerm $ (pmap # (plam pdata) # pconstant l)
    check :: Term s (PBuiltinList (PAsData PInteger) :--> PBool)
    check = plam $ \list -> puniqueOrdered # (plam $ pdata) # 0 # list #== list

property_puniqueOrdered_errorOnDuplicates :: Property
property_puniqueOrdered_errorOnDuplicates = forAll indexList $ fromFailingPPartial $ check
  where
    atLeastOneElement [] = [1]
    atLeastOneElement xs = xs
    indexList :: Gen (TestableTerm (PBuiltinList (PAsData PInteger)))
    indexList = do
      n <- chooseInteger (1, 100)
      s <- atLeastOneElement <$> sublistOf [1 .. n]
      r <- chooseInt (0, (length s) - 1)
      l <- shuffle ((s !! r) : s)
      return $ TestableTerm $ (pmap # (plam pdata) # pconstant l)
    check :: Term s (PBuiltinList (PAsData PInteger) :--> POpaque)
    check = plam $ \list -> popaque $ puniqueOrdered # (plam $ pdata) # 0 # list

alice :: Credential
alice = PubKeyCredential "86ae9eebd8b97944a45201e4aec1330a72291af2d071644bba015959"

correctRouterRedeemer :: RouterRedeemer
correctRouterRedeemer =
  RouterRedeemer
    { inputIdxs = [0]
    , outputIdxs = [0]
    }

negativeIndicesRouterRedeemer :: RouterRedeemer
negativeIndicesRouterRedeemer =
  RouterRedeemer
    { inputIdxs = [-1]
    , outputIdxs = [0]
    }

routerScript :: Script
routerScript = fromRight undefined $ compileTerm $ smartHandleRouteValidatorW

stakingScript :: Script
stakingScript = fromRight undefined $ compileTerm $ pstakeValidator # minSwapAddress

stakingCredential :: StakingCredential
stakingCredential = StakingHash $ ScriptCredential $ scriptHash $ stakingScript

minAssetClass :: AssetClass
minAssetClass = assetClass "e16c2dc8ae937e8d3790c7fd7168d7b994621ba14ca11415f39fed72" "MIN"

inputTxOutRef :: TxOutRef
inputTxOutRef = TxOutRef (TxId "0000000000000000") 0

inputRedeemer :: SmartRedeemer
inputRedeemer = RouteSmart

scriptInput :: (Builder a) => a
scriptInput =
  input $
    mconcat
      [ script $ scriptHash routerScript
      , withRef inputTxOutRef
      , withValue (singleton adaSymbol adaToken 10_000_000)
      , withRedeemer inputRedeemer
      , withDatum $
          Advanced
            (Just $ Address alice Nothing)
            1_000_000
            0
            None
            ( toBuiltinData $
                MinswapRequestInfo
                  (fst $ unAssetClass minAssetClass)
                  (snd $ unAssetClass minAssetClass)
                  Nothing
                  10
            )
      ]

scriptOutput :: (Builder a) => a
scriptOutput =
  output $
    mconcat
      [ address $ plift minSwapAddress
      , withValue (singleton adaSymbol adaToken 9_000_000)
      , withRedeemer RouteSmart
      , withDatum $
          MinswapRequestDatum
            { sender = Address alice Nothing
            , receiver = Address alice Nothing
            , receiverDatumHash = Nothing
            , step = OrderType {desiredAsset = minAssetClass, minReceive = 10}
            , batcherFee = 2_000_000
            , outputAda = 2_000_000
            }
      ]

scriptContextWithNegativeIndex :: RouterRedeemer -> ScriptContext
scriptContextWithNegativeIndex withdrawRedeemer =
  buildRewarding' $
    mconcat
      [ extraRedeemer (Rewarding stakingCredential) withdrawRedeemer
      , extraRedeemer (Spending inputTxOutRef) inputRedeemer
      , withdrawal stakingCredential 0
      , scriptInput
      , scriptOutput
      ]

stakingValidatorTests :: TestTree
stakingValidatorTests = tryFromPTerm "Staking validator" (pstakeValidator # minSwapAddress) $ do
  [toData correctRouterRedeemer, toData (trace (show (scriptContextWithNegativeIndex correctRouterRedeemer)) (scriptContextWithNegativeIndex correctRouterRedeemer))] @> "accepts correct index"
  [toData negativeIndicesRouterRedeemer, toData (scriptContextWithNegativeIndex negativeIndicesRouterRedeemer)] @!> "does not accept negative index"
