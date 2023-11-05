module Main where

import Samples.Simple qualified as Simple
import Utils (writePlutusScript)

main :: IO ()
main = do
  writePlutusScript "gift" "./compiled/gift.plutus" Simple.gift