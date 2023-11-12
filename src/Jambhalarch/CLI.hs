module Jambhalarch.CLI (runJamb) where

import Cardano.Binary (serialize')
import Control.Monad (when)
import Control.Monad.Reader (MonadIO (..), MonadReader, ReaderT (..), asks)
import Data.Aeson (ToJSON (..))
import Data.Aeson qualified as Aeson
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Aeson.Encode.Pretty qualified as AEP
import Data.Bifunctor (first)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Base16 qualified as Base16
import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Lazy qualified as LBS
import Data.Char (toUpper)
import Data.Foldable (traverse_)
import Data.Map.Strict qualified as M
import Data.Maybe (fromMaybe)
import Data.Text (Text, pack)
import Data.Text.Encoding (decodeUtf8)
import Jambhalarch.CLI.Parsers (commandParser, contractsPretty)
import Jambhalarch.CLI.Types (
  Command (..),
  ContractExports (ContractExports),
  DataExport (..),
  DefDir,
  EnvVar,
  FileExt,
  FileName,
  JambContracts,
  PlutusFile (..),
  ScriptData (..),
 )
import Options.Applicative (execParser)
import Plutarch (
  ClosedTerm,
  Config (Config),
  Script,
  TracingMode (DoTracing),
  compile,
 )
import Plutarch.Evaluate (evalScript)
import Plutarch.Extra.Script (applyArguments)
import Plutarch.Script (serialiseScript)
import PlutusLedgerApi.V2 (ExBudget)
import PlutusTx (Data, ToData, toData)
import PlutusTx qualified as Plutus
import System.Directory.Extra (doesDirectoryExist, doesFileExist)
import System.Environment (lookupEnv)
import System.Exit (exitFailure, exitSuccess)
import System.IO (BufferMode (..), hPutStrLn, hSetBuffering, stderr, stdin, stdout)
import Test.Tasty (TestTree, defaultIngredients)
import Test.Tasty.Ingredients (tryIngredients)
import Text.Printf (printf)

runJamb :: MonadIO m => JambContracts -> m ()
runJamb = runReaderT (commandParser >>= liftIO . execParser >>= runCommand)

runCommand :: (MonadReader JambContracts m, MonadIO m) => Command -> m ()
runCommand = \case
  List -> asks contractsPretty >>= liftIO . putStrLn . ("Available Contracts:\n\n" ++)
  Test c ->
    go c (liftIO . defaultMainNoOpts . \(ContractExports _ _ test) -> test)
  Write c mfn -> go c (writeScriptWithData (fromMaybe c mfn))
  where
    go contract eff =
      asks (M.lookup contract)
        >>= maybe (liftIO . putStrLn $ "Error: contract \"" ++ contract ++ "\" not found") eff

writeScriptWithData :: MonadIO m => FileName -> ContractExports -> m ()
writeScriptWithData fn (ContractExports s ds _) = do
  _ <- liftIO $ writeScriptToFile fn s
  liftIO $ traverse_ writeDataToFile ds

writeScriptToFile :: MonadIO m => FileName -> ClosedTerm a -> m ()
writeScriptToFile fn term = do
  fp <- liftIO $ mkFilePath "PLUTUS_SCRIPTS_PATH" "cardano-cli-guru/assets/scripts/plutus" fn ".plutus"
  fileOverwritePrompt fp $ case evalT term of
    Left e -> liftIO . putStrLn $ show e
    Right (script, _, _) -> do
      let pf = PlutusFile "PlutusScriptV2" fn $ encodeSerialiseCBOR script
          cfg =
            AEP.defConfig
              { AEP.confCompare = AEP.keyOrder ["type", "description", "cborHex"]
              }
      liftIO . putStrLn $ "Wrote script to '" ++ fp ++ "'"
      liftIO . LBS.writeFile fp $ AEP.encodePretty' cfg pf

mkFilePath :: MonadIO m => EnvVar -> DefDir -> FileName -> FileExt -> m FilePath
mkFilePath envVar defDir fn ext = do
  mDir <- liftIO $ lookupEnv envVar
  case mDir >>= stripTrailingSlash of
    Nothing -> do
      liftIO . putStrLn $ mconcat [envVar, " not set. Using default directory '", defDir, "'."]
      mkDefFilePath
    Just dir -> do
      envDirExists <- liftIO $ doesDirectoryExist dir
      if envDirExists
        then pure $ mkFilePath' dir
        else do
          liftIO . putStrLn $ mconcat ["'", dir, "' does not exist. Using default directory '", defDir, "'."]
          mkDefFilePath
  where
    stripTrailingSlash :: String -> Maybe String
    stripTrailingSlash [] = Nothing
    stripTrailingSlash fp = if last fp == '/' then Just (init fp) else Just fp

    mkFilePath' :: String -> String
    mkFilePath' dir =
      concat
        [ dir
        , "/"
        , fn
        , ext
        ]

    mkDefFilePath :: MonadIO m => m FilePath
    mkDefFilePath = do
      defDirExists <- liftIO $ doesDirectoryExist defDir
      if defDirExists
        then pure $ mkFilePath' defDir
        else error $ concat ["Error: default directory '", defDir, "' does not exist."]

fileOverwritePrompt :: MonadIO m => FileName -> m () -> m ()
fileOverwritePrompt fn writeAction = do
  liftIO $ hSetBuffering stdin NoBuffering
  liftIO $ hSetBuffering stdout NoBuffering
  fileExists <- liftIO $ doesFileExist fn
  if fileExists
    then do
      liftIO $ putStr $ "File '" ++ fn ++ "' already exists. Do you want to overwrite it? (Y/N): "
      response <- liftIO getChar
      liftIO $ putStrLn ""
      case toUpper response of
        'Y' -> writeAction
        'N' -> pure ()
        _ -> liftIO (putStrLn "Invalid response. Please enter Y or N.") >> fileOverwritePrompt fn writeAction
    else writeAction

writeDataToFile :: MonadIO m => DataExport -> m ()
writeDataToFile (DataExport fn d) = do
  fp <- mkFilePath "DATA_PATH" "cardano-cli-guru/assets/data" fn ".json"
  let v = dataToJSON d
  fileOverwritePrompt fp $ liftIO (writeFileJSON fp v)
  where
    dataToJSON :: ToData a => a -> Aeson.Value
    dataToJSON = toJSON . fromPlutusData . toData

fromPlutusData :: Plutus.Data -> ScriptData
fromPlutusData (Plutus.Constr int xs) =
  ScriptDataConstructor
    int
    [fromPlutusData x | x <- xs]
fromPlutusData (Plutus.Map kvs) =
  ScriptDataMap
    [ (fromPlutusData k, fromPlutusData v)
    | (k, v) <- kvs
    ]
fromPlutusData (Plutus.List xs) =
  ScriptDataList
    [fromPlutusData x | x <- xs]
fromPlutusData (Plutus.I n) = ScriptDataNumber n
fromPlutusData (Plutus.B bs) = ScriptDataBytes bs

serialiseToJSON :: ToJSON a => a -> ByteString
serialiseToJSON = LBS.toStrict . Aeson.encode

prettyPrintJSON :: ToJSON a => a -> ByteString
prettyPrintJSON = LBS.toStrict . encodePretty

encodeSerialiseCBOR :: Script -> Text
encodeSerialiseCBOR = decodeUtf8 . Base16.encode . serialize' . serialiseScript

evalWithArgsT :: [Data] -> ClosedTerm a -> Either Text (Script, ExBudget, [Text])
evalWithArgsT args x = do
  cmp <- compile (Config DoTracing) x
  let (escr, budg, trc) = evalScript $ applyArguments cmp args
  scr <- first (pack . show) escr
  pure (scr, budg, trc)

evalT :: ClosedTerm a -> Either Text (Script, ExBudget, [Text])
evalT = evalWithArgsT []

writeFileJSON ::
  ToJSON a =>
  FilePath ->
  a ->
  IO ()
writeFileJSON path x = do
  BS.writeFile path (serialiseToJSON x)
  printf "Wrote data to '%s':\n\n%s\n\n" path . BS8.unpack $ prettyPrintJSON x

{- | Reimplementation of `defaultMain` that doesn't check command-line args
    (for compatibility with `jamb` CLI)
-}
defaultMainNoOpts :: TestTree -> IO ()
defaultMainNoOpts testTree = do
  case tryIngredients defaultIngredients mempty testTree of
    Nothing -> do
      hPutStrLn
        stderr
        "No ingredients agreed to run. Something is wrong either with your ingredient set or the options."
      exitFailure
    Just ioRes ->
      ioRes >>= \res -> when res exitSuccess >> exitFailure