module SmartHandlesSpec (tests) where

import Plutarch
import Plutarch.Prelude
import Plutarch.Test.QuickCheck (TestableTerm (..), fromFailingPPartial, fromPFun)

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (Gen, Property, chooseInt, chooseInteger, forAll, shuffle, sublistOf, testProperty)

import SmartHandles

tests :: TestTree
tests =
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
