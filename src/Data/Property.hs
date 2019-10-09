{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Data.Property
  ( Property'(..)
  , Property
  , PropertyId
  , idL
  , nameL
  , aliasesL
  , descriptionL
  , refsL
  ) where

import Import

import Data.Aeson     (FromJSON(..), ToJSON(..))
import Data.Citation  (Citation)
import Data.Id        as Id (Id, Encodable(..), parseJSON, toJSON)
import Data.Structure (HKD, LensFor(..), getLenses)

type PropertyId = Id Property

data Property' f = Property
  { id          :: HKD f PropertyId
  , name        :: HKD f Text
  , aliases     :: HKD f [Text]
  , description :: HKD f Text
  , refs        :: HKD f [Citation]
  } deriving Generic

Property
  (LensFor idL)
  (LensFor nameL)
  (LensFor aliasesL)
  (LensFor descriptionL)
  (LensFor refsL)
  = getLenses

type Property = Property' Identity

deriving instance Show Property
deriving instance Eq   Property

instance ToJSON PropertyId where
  toJSON = Id.toJSON 'P'

instance FromJSON PropertyId where
  parseJSON = Id.parseJSON "P"

instance Id.Encodable Property where
  prefix = 'P'