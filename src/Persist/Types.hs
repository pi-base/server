{-# LANGUAGE DuplicateRecordFields #-}
module Persist.Types
  ( CreateProperty(..)
  , CreateSpace(..)
  , CreateTheorem(..)
  , CreateTrait(..)
  , UpdateProperty(..)
  , UpdateSpace(..)
  , UpdateTheorem(..)
  , UpdateTrait(..)
  ) where

import Core

import qualified Data.Trait as Trait (Value)

data CreateSpace = CreateSpace
  { name        :: Text
  , aliases     :: [Text]
  , description :: Text
  , topology    :: Maybe Text
  , refs        :: [Citation]
  } deriving (Generic, Eq, Show)

data UpdateSpace = UpdateSpace
  { id          :: SpaceId
  , name        :: Text
  , aliases     :: [Text]
  , description :: Text
  , topology    :: Maybe Text
  , refs        :: [Citation]
  } deriving (Generic, Eq, Show)

data CreateProperty = CreateProperty
  { name        :: Text
  , aliases     :: [Text]
  , description :: Text
  , refs        :: [Citation]
  } deriving (Generic, Eq, Show)

data UpdateProperty = UpdateProperty
  { id          :: PropertyId
  , name        :: Text
  , aliases     :: [Text]
  , description :: Text
  , refs        :: [Citation]
  } deriving (Generic, Eq, Show)

data CreateTheorem = CreateTheorem
  { implication :: Implication PropertyId
  , converse    :: Maybe [TheoremId]
  , description :: Text
  , refs        :: [Citation]
  } deriving (Generic, Eq, Show)

data UpdateTheorem = UpdateTheorem
  { id          :: TheoremId
  , converse    :: Maybe [TheoremId]
  , description :: Text
  , refs        :: [Citation]
  } deriving (Generic, Eq, Show)

data CreateTrait = CreateTrait
  { id          :: TraitId
  , value       :: Trait.Value
  , description :: Text
  , refs        :: [Citation]
  } deriving (Generic, Eq, Show)

data UpdateTrait = UpdateTrait
  { id          :: TraitId
  , description :: Text
  , refs        :: [Citation]
  } deriving (Generic, Eq, Show)
