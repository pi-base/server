module Data.Match
  ( Match(..)
  ) where

import Import

data Match = Yes | No | Unknown
  deriving (Show, Eq, Ord)
