module Data.Commit
  ( Commit(..)
  ) where

import Import

import Data.User (User)

data Commit = Commit
  { user    :: User
  , message :: Text
  } deriving (Generic, Eq, Ord, Show)
