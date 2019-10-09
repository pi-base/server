module Data.Citation
  ( Citation(..)
  , CitationType(..)
  ) where

import Import

data CitationType = DOICitation | MRCitation | WikiCitation
  deriving (Generic, Show, Eq, Ord)

data Citation = Citation
  { citationName :: Text
  , citationType :: CitationType
  , citationRef  :: Text
  } deriving (Generic, Show, Eq, Ord)
