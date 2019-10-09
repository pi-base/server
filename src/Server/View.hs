{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Server.View
  ( View'(..)
  , View
  , build
  , propertiesL
  , spacesL
  , theoremsL
  , traitsL
  , versionL
  ) where

import Core

import Data.Structure (HKD, LensFor(..), getLenses)

data View' f = View
  { properties :: HKD f [Property]
  , spaces     :: HKD f [Space]
  , theorems   :: HKD f [Theorem]
  , traits     :: HKD f [Trait]
  , version    :: HKD f Version
  } deriving Generic

View
  (LensFor propertiesL)
  (LensFor spacesL)
  (LensFor theoremsL)
  (LensFor traitsL)
  (LensFor versionL)
  = getLenses

type View = View' Identity

deriving instance Show View
deriving instance Eq   View

build :: [Property] -> [Space] -> [Theorem] -> [Trait] -> Version -> View
build = View
