{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
module Data.Access
  ( Access(..)
  ) where

import Import

import Data.Text              (unpack)
import Database.Beam
import Database.Beam.Postgres
import Prelude                (read)

data Access = None | Read | Write | Admin
  deriving (Show, Read, Eq, Ord, Enum, Generic)

instance FromBackendRow Postgres Access where
  fromBackendRow = read . unpack <$> fromBackendRow
