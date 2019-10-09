{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Data.Trait
  ( Trait'(..)
  , Trait
  , TraitId
  , Value(..)
  , id
  , idL
  , spaceL
  , propertyL
  , valueL
  , descriptionL
  , refsL
  ) where

import Import

import Control.Lens   (lens)
import Data.Citation  (Citation)
import Data.Property  as Property (PropertyId)
import Data.Space     as Space (SpaceId)
import Data.Structure (HKD, LensFor(..), getLenses)

newtype Value = Value Bool
  deriving (Generic, Show, Eq, Ord, FromJSON, ToJSON)

type TraitId = (SpaceId, PropertyId)

data Trait' f = Trait
  { space       :: HKD f SpaceId
  , property    :: HKD f PropertyId
  , value       :: HKD f Value
  , description :: HKD f Text
  , refs        :: HKD f [Citation]
  } deriving Generic

type Trait = Trait' Identity

deriving instance Show Trait
deriving instance Eq   Trait

Trait
  (LensFor spaceL)
  (LensFor propertyL)
  (LensFor valueL)
  (LensFor descriptionL)
  (LensFor refsL)
  = getLenses

id :: Trait -> TraitId
id Trait{..} = (space, property)

idL :: Lens' Trait TraitId
idL = lens
  (\t -> (space t, property t))
  (\t (s, p) -> t { space = s, property = p })
