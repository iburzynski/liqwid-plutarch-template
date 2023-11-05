module Utils where

import Cardano.Binary (serialize')
import Data.Aeson
import Data.Aeson.Encode.Pretty qualified as AEP
import Data.Bifunctor (first)
import Data.ByteString.Base16 qualified as Base16
import Data.ByteString.Lazy qualified as LBS
import Data.Char (toLower)
import Data.Text (Text, pack)
import Data.Text.Encoding (decodeUtf8)
import GHC.Generics (Generic)
import Plutarch
import Plutarch.Evaluate (evalScript)
import Plutarch.Extra.Script (applyArguments)
import Plutarch.Script (serialiseScript)
import PlutusLedgerApi.V2 (ExBudget)
import PlutusTx (Data)

data PlutusFile = PlutusFile
  { pfType :: String
  , pfDescription :: String
    , pfCborHex :: Text
  }
  deriving (Show, Generic)

instance ToJSON PlutusFile where
  toJSON = genericToJSON defaultOptions {fieldLabelModifier = lowerFirst . drop 2}
    where
      lowerFirst (x : xs) = toLower x : xs
      lowerFirst [] = []

encodeSerialiseCBOR :: Script -> Text
encodeSerialiseCBOR = decodeUtf8 . Base16.encode . serialize' . serialiseScript

evalT :: ClosedTerm a -> Either Text (Script, ExBudget, [Text])
evalT x = evalWithArgsT x []

evalWithArgsT :: ClosedTerm a -> [Data] -> Either Text (Script, ExBudget, [Text])
evalWithArgsT x args = do
  cmp <- compile (Config DoTracing) x
  let (escr, budg, trc) = evalScript $ applyArguments cmp args
  scr <- first (pack . show) escr
  pure (scr, budg, trc)

writePlutusScript :: String -> FilePath -> ClosedTerm a -> IO ()
writePlutusScript title filepath term = do
  case evalT term of
    Left e -> putStrLn (show e)
    Right (script, _, _) -> do
      let pf = PlutusFile "PlutusScriptV2" title $ encodeSerialiseCBOR script
          cfg =
            AEP.defConfig
              { AEP.confCompare = AEP.keyOrder ["type", "description", "cborHex"]
              }
      LBS.writeFile filepath $ AEP.encodePretty' cfg pf
