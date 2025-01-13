{-# LANGUAGE OverloadedLists #-}

{- HLINT ignore "Use empty" -}

module Contracts (contracts, samples) where

import Contracts.Samples.Simple qualified as Simple
import Jambhalarch.CLI.Types (JambContracts)

contracts :: JambContracts
contracts = []

samples :: JambContracts
samples =
  [ Simple.giftExports
  , Simple.burnExports
  ]
