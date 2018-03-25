module Services.Rollbar.Types
  ( Level(..)
  , Settings(..)
  ) where

import Prelude
import Data.Text (Text)

data Level = Debug | Info | Warning | Error | Critical
  deriving (Ord, Eq, Show)

data Settings = Settings
  { token       :: Maybe Text
  , environment :: Text
  , hostname    :: Text
  , active      :: Bool
  , build       :: Text
  }
