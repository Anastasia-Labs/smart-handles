module Main (main) where

import SmartHandlesSpec qualified as SmartHandlesSpec
import Test.Tasty (defaultMain, testGroup)

main :: IO ()
main =
  defaultMain $ testGroup "smart-handles" [SmartHandlesSpec.tests]
