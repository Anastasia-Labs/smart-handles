module Main (main) where

import Data.Aeson (KeyValue ((.=)), object)
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Bifunctor (first)
import Data.ByteString.Base16 qualified as Base16
import Data.ByteString.Lazy qualified as LBS
import Data.Text (Text, pack)
import Data.Text.Encoding qualified as Text
import System.Console.ANSI (Color (..), ColorIntensity (..), ConsoleLayer (..), SGR (..), setSGR)

import Cardano.Binary qualified as CBOR
import PlutusLedgerApi.V2 (Data, ExBudget)

import Plutarch.Evaluate (evalScript)
import Plutarch.Prelude
import Plutarch.Script (Script, serialiseScript)
import "liqwid-plutarch-extra" Plutarch.Extra.Script (applyArguments)

import BatchValidator (smartHandleRouteValidatorW)
import Compilation
import MinSwap.AdaToMin

encodeSerialiseCBOR :: Script -> Text
encodeSerialiseCBOR = Text.decodeUtf8 . Base16.encode . CBOR.serialize' . serialiseScript

evalT :: ClosedTerm a -> Either Text (Script, ExBudget, [Text])
evalT x = evalWithArgsT x []

evalWithArgsT :: ClosedTerm a -> [Data] -> Either Text (Script, ExBudget, [Text])
evalWithArgsT x args = do
  cmp <- compileTerm x
  let (escr, budg, trc) = evalScript $ applyArguments cmp args
  scr <- first (pack . show) escr
  pure (scr, budg, trc)

writePlutusScript :: String -> FilePath -> ClosedTerm a -> IO ()
writePlutusScript title filepath term = do
  case evalT term of
    Left e -> putStrLn (show e)
    Right (script, _, _) -> do
      let scriptType = "PlutusScriptV2" :: String
          plutusJson = object ["type" .= scriptType, "description" .= title, "cborHex" .= encodeSerialiseCBOR script]
          content = encodePretty plutusJson
      LBS.writeFile filepath content

main :: IO ()
main = do
  setSGR [SetColor Foreground Vivid Blue]
  putStrLn "Exporting Plutarch scripts..."
  setSGR [Reset]

  writePlutusScript "Smart Handle" "./compiled/smartHandleSimple.json" psingleValidator
  putStrLn "Exported smart handle validator"

  writePlutusScript "Smart Handle Router" "./compiled/smartHandleRouter.json" smartHandleRouteValidatorW
  putStrLn "Exported smart handle router validator"

  writePlutusScript "Smart Handle Router" "./compiled/smartHandleStake.json" pstakeValidator
  putStrLn "Exported smart handle stake validator"

  setSGR [SetColor Foreground Vivid Green]
  putStrLn "Done exporting Plutarch scripts, have a great day!"
  setSGR [Reset]
