module Data.Commit
  ( Commit(..)
  ) where

import Import
import Persist.Backend.DB.Model (User)

data Commit = Commit
  { user    :: User
  , message :: Text
  } deriving (Generic, Eq, Ord, Show)
