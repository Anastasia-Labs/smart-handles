module SmartHandlesSpec (tests) where

import Data.Either (fromRight)

import PlutusLedgerApi.V1.Value (AssetClass, assetClass)
import PlutusLedgerApi.V2 (Address (..), Credential (..), ScriptContext, StakingCredential (..), adaSymbol, adaToken, singleton)
import PlutusTx (toData)

import Plutarch
import Plutarch.Api.V2 (scriptHash)
import Plutarch.Context (Builder, address, buildRewarding', input, output, script, withDatum, withRedeemer, withValue, withdrawal)
import Plutarch.Prelude
import Plutarch.Test.Precompiled (tryFromPTerm, (@!>), (@>))
import Plutarch.Test.QuickCheck (TestableTerm (..), fromFailingPPartial, fromPFun)

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (Gen, Property, chooseInt, chooseInteger, forAll, shuffle, sublistOf, testProperty)

import Compilation
import SmartHandles

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
stakingScript = fromRight undefined $ compileTerm $ smartHandleStakeValidatorW # swapAddress

stakingCredential :: StakingCredential
stakingCredential = StakingHash $ ScriptCredential $ scriptHash $ stakingScript

scriptInput :: (Builder a) => a
scriptInput =
  input $
    mconcat
      [ script $ scriptHash routerScript
      , withValue (singleton adaSymbol adaToken 10_000_000)
      , withRedeemer SwapSmart
      , withDatum $ Address alice Nothing
      ]

minAssetClass :: AssetClass
minAssetClass = assetClass "29d222ce763455e3d7a09a665ce554f00ac89d2e99a1a83d267170c6" "MIN"

scriptOutput :: (Builder a) => a
scriptOutput =
  output $
    mconcat
      [ address $ plift swapAddress
      , withValue (singleton adaSymbol adaToken 9_000_000)
      , withRedeemer SwapSmart
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

scriptContextWithNegativeIndex :: ScriptContext
scriptContextWithNegativeIndex =
  buildRewarding' $
    mconcat
      [ withdrawal stakingCredential 0
      , scriptInput
      , scriptOutput
      ]

stakingValidatorTests :: TestTree
stakingValidatorTests = tryFromPTerm "Staking validator" (smartHandleStakeValidatorW # swapAddress) $ do
  [toData correctRouterRedeemer, toData scriptContextWithNegativeIndex] @> "accepts correct index"
  [toData negativeIndicesRouterRedeemer, toData scriptContextWithNegativeIndex] @!> "does not accept negative index"
