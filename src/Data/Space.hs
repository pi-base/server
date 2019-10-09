{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Data.Space
  ( Space'(..)
  , Space
  , SpaceId
  , idL
  , nameL
  , aliasesL
  , descriptionL
  , topologyL
  , refsL
  ) where

import Import

import Data.Aeson     (FromJSON(..), ToJSON(..))
import Data.Citation  (Citation)
import Data.Id        as Id (Id(..), Encodable(..), parseJSON, toJSON)
import Data.Structure (HKD, LensFor(..), getLenses)

type SpaceId = Id Space

data Space' f = Space
  { id          :: HKD f SpaceId
  , name        :: HKD f Text
  , aliases     :: HKD f [Text]
  , description :: HKD f Text
  , topology    :: HKD f (Maybe Text)
  , refs        :: HKD f [Citation]
  } deriving Generic

Space
  (LensFor idL)
  (LensFor nameL)
  (LensFor aliasesL)
  (LensFor descriptionL)
  (LensFor topologyL)
  (LensFor refsL)
  = getLenses

type Space = Space' Identity

deriving instance Show Space
deriving instance Eq   Space

instance ToJSON SpaceId where
  toJSON = Id.toJSON 'S'

instance FromJSON SpaceId where
  parseJSON = Id.parseJSON "S"

instance Id.Encodable Space where
  prefix = 'S'
