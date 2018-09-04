{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}
module Test.Fixtures
  ( module Test.Fixtures
  ) where

import Protolude

import Core
import Data.Id as Id

-- This is dumb hack so that we can use int literals as Id types in test
instance Identifiable a => Num (Id a) where
  fromInteger = Id.fromInt

compact, metacompact, metrizable :: PropertyId
compact     = 16
metacompact = 31
metrizable  = 53

finiteDiscrete, indiscrete :: SpaceId
finiteDiscrete = 1
indiscrete     = 4

finiteIsCompact :: TheoremId
finiteIsCompact = 198