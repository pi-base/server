{-# LANGUAGE DuplicateRecordFields #-}
module Server.Types
  ( CreatePropertyBody(..)
  , CreateSpaceBody(..)
  , CreateTheoremBody(..)
  , CreateTraitBody(..)
  , Status(..)
  , UpdatePropertyBody(..)
  , UpdateSpaceBody(..)
  , UpdateTheoremBody(..)
  , UpdateTraitBody(..)
  ) where

import           Core
import qualified Data.Trait    as Trait
import           Server.Status (Status(..))

data CreateSpaceBody = CreateSpaceBody
  { name        :: Text
  , aliases     :: [Text]
  , description :: Text
  , topology    :: Maybe Text
  , refs        :: [Citation]
  } deriving (Generic, Eq, Show)

data UpdateSpaceBody = UpdateSpaceBody
  { name        :: Text
  , aliases     :: [Text]
  , description :: Text
  , topology    :: Maybe Text
  , refs        :: [Citation]
  } deriving (Generic, Eq, Show)

data CreatePropertyBody = CreatePropertyBody
  { name        :: Text
  , aliases     :: [Text]
  , description :: Text
  , refs        :: [Citation]
  } deriving (Generic, Eq, Show)

data UpdatePropertyBody = UpdatePropertyBody
  { name        :: Text
  , aliases     :: [Text]
  , description :: Text
  , refs        :: [Citation]
  } deriving (Generic, Eq, Show)

data CreateTheoremBody = CreateTheoremBody
  { implication :: Implication PropertyId
  , converse    :: Maybe [TheoremId]
  , description :: Text
  , refs        :: [Citation]
  } deriving (Generic, Eq, Show)

data UpdateTheoremBody = UpdateTheoremBody
  { converse    :: Maybe [TheoremId]
  , description :: Text
  , refs        :: [Citation]
  } deriving (Generic, Eq, Show)

data CreateTraitBody = CreateTraitBody
  { value       :: Trait.Value
  , description :: Text
  , refs        :: [Citation]
  } deriving (Generic, Eq, Show)

data UpdateTraitBody = UpdateTraitBody
  { description :: Text
  , refs        :: [Citation]
  } deriving (Generic, Eq, Show)
