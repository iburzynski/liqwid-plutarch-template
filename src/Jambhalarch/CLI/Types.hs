module Jambhalarch.CLI.Types (
  Command (..),
  ContractExports (..),
  ContractName,
  DataExport (..),
  DefDir,
  EnvVar,
  ExportTemplate (..),
  FileExt,
  FileName,
  JambContracts,
  JambExports,
  PlutusFile (..),
  ScriptData (..),
  TestTree,
) where

import Data.Aeson (Options (fieldLabelModifier), ToJSON (..), defaultOptions, genericToJSON)
import Data.Aeson qualified as Aeson
import Data.ByteString qualified as BS
import Data.ByteString.Base16 qualified as Base16
import Data.Char (toLower)
import Data.Map.Strict (Map)
import Data.Text (Text)
import Data.Text.Encoding qualified as Text
import Data.Vector qualified as Vector
import GHC.Generics (Generic)
import Plutarch.Prelude (ClosedTerm)
import PlutusTx (ToData)
import Test.Tasty (TestTree)

-- | A string identifier used to reference an exported contract in the `jamb` CLI.
type ContractName = String

-- | The target filename for a written .plutus file, without extension.
type FileName = String

-- | Internal representation of `jamb` CLI.
data Command
  = List
  | Test !ContractName
  | Write !ContractName !(Maybe FileName)

data ContractExports where
  ContractExports ::
    { script :: !(ClosedTerm a)
    , dExports :: ![DataExport]
    , tastyTest :: !TestTree
    } ->
    ContractExports

{- | A pair containing a contract's name and its exported data.
   `JambExports` values can be imported into the `Contracts` module and added to the list of contracts for use with the `jamb` CLI.
-}
type JambExports = (ContractName, ContractExports)

{- | An intermediary record type used to prepare scripts and their associated data for
   conversion to a `JambExports` value.
-}
data ExportTemplate where
  ExportTemplate ::
    { contractName :: !ContractName
    , scriptExport :: !(ClosedTerm a)
    , dataExports :: ![DataExport]
    , test :: !TestTree
    } ->
    ExportTemplate

{- | A `Map` between `ContractName` keys and `ContractExports` values.
   Used by the `jamb` CLI to look up and operate on available contracts.
-}
type JambContracts = Map ContractName ContractExports

{- | Additional input data to accompany a contract.
   Serialised to a `.json` file by the `jamb -w` command.
-}
data DataExport where
  DataExport :: (ToData d) => String -> d -> DataExport

-- | Representation of Plutus script data for conversion to JSON
data ScriptData
  = ScriptDataConstructor
      Integer
      [ScriptData]
  | ScriptDataMap [(ScriptData, ScriptData)]
  | ScriptDataList [ScriptData]
  | ScriptDataNumber Integer
  | ScriptDataBytes BS.ByteString
  deriving (Eq, Ord, Show)

instance ToJSON ScriptData where
  toJSON :: ScriptData -> Aeson.Value
  toJSON sData = case sData of
    (ScriptDataNumber n) ->
      singleFieldObject "int"
        . Aeson.Number
        $ fromInteger n
    (ScriptDataBytes bs) ->
      singleFieldObject "bytes"
        . Aeson.String
        $ Text.decodeLatin1 (Base16.encode bs)
    (ScriptDataList vs) ->
      singleFieldObject "list"
        . Aeson.Array
        $ Vector.fromList (map toJSON vs)
    (ScriptDataMap kvs) ->
      singleFieldObject "map"
        . Aeson.Array
        $ Vector.fromList
          [ Aeson.object [("k", toJSON k), ("v", toJSON v)]
          | (k, v) <- kvs
          ]
    (ScriptDataConstructor n vs) ->
      Aeson.object
        [ ("constructor", Aeson.Number (fromInteger n))
        , ("fields", Aeson.Array (Vector.fromList (map toJSON vs)))
        ]
    where
      singleFieldObject name v = Aeson.object [(name, v)]

-- | Representation of .plutus files for conversion to JSON
data PlutusFile = PlutusFile
  { pfType :: String
  , pfDescription :: String
  , pfCborHex :: Text
  }
  deriving (Show, Generic)

instance ToJSON PlutusFile where
  toJSON :: PlutusFile -> Aeson.Value
  toJSON = genericToJSON defaultOptions {fieldLabelModifier = lowerFirst . drop 2}
    where
      lowerFirst (x : xs) = toLower x : xs
      lowerFirst [] = []

type EnvVar = String

type DefDir = String

type FileExt = String