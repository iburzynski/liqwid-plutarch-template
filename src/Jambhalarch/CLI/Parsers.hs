module Jambhalarch.CLI.Parsers (commandParser, contractsPretty) where

import Control.Monad.Reader (MonadReader (..))
import Data.Map.Strict qualified as M
import Jambhalarch.CLI.Types
import Options.Applicative

commandParser :: (MonadReader JambContracts m) => m (ParserInfo Command)
commandParser = do
  pw <- parseWrite
  pt <- parseTest
  let p = parseList <|> pt <|> pw
  pure . info (helper <*> p) $ mconcat [fullDesc, progDesc "Jambhalarch Cardano Development Suite"]

parseList :: Parser Command
parseList = flag' List (long "list" <> short 'l' <> help "List the available contracts")

parseTest :: (MonadReader JambContracts m) => m (Parser Command)
parseTest =
  fmap (fmap Test) . parseContractName $
    mconcat
      [ long "test"
      , short 't'
      , metavar "CONTRACT"
      , help "Run tests for CONTRACT"
      ]

parseWrite :: (MonadReader JambContracts m) => m (Parser Command)
parseWrite =
  fmap ((<*> parseFName) . fmap Write) . parseContractName $
    mconcat
      [ long "write"
      , short 'w'
      , metavar "CONTRACT"
      , help "Write CONTRACT to file with optional FILENAME (default is CONTRACT.plutus) and mainnet flag"
      ]
  where
    parseFName = optional $ argument str (metavar "FILENAME")

parseContractName :: (MonadReader JambContracts m) => Mod OptionFields String -> m (Parser String)
parseContractName fields = do
  contracts <- ask
  let readMName = eitherReader $ \c ->
        if c `M.member` contracts
          then Right c
          else
            Left $
              "Error: contract not found - choose one of the contracts below:\n\n"
                ++ contractsPretty contracts
  pure $ option readMName fields

contractsPretty :: JambContracts -> String
contractsPretty = unlines . map ('\t' :) . M.keys
