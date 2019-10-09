{-# LANGUAGE TemplateHaskell #-}
module Data.Access
  ( Access(..)
  ) where

import Import
import Database.Persist.TH

data Access = None | Read | Write | Admin
  deriving (Show, Read, Eq, Ord, Enum, Generic)

derivePersistField "Access"