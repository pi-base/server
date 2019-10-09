{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Data.Theorem
  ( Theorem'(..)
  , Theorem
  , TheoremId
  , converseL
  , descriptionL
  , idL
  , if'
  , implicationL
  , properties
  , refsL
  , then'
  ) where

import Import

import           Data.Aeson       (FromJSON(..), ToJSON(..))
import           Data.Citation    (Citation)
import           Data.Formula     (Formula)
import           Data.Id          as Id (Id, Encodable(..), parseJSON, toJSON)
import qualified Data.Implication as Implication
import           Data.Implication (Implication(..))
import           Data.Property    as Property (PropertyId)
import           Data.Structure   (HKD, LensFor(..), getLenses)

type TheoremId = Id Theorem

data Theorem' f = Theorem
  { id          :: HKD f TheoremId
  , implication :: HKD f (Implication PropertyId)
  , converse    :: HKD f (Maybe [TheoremId])
  , description :: HKD f Text
  , refs        :: HKD f [Citation]
  } deriving Generic

Theorem
  (LensFor idL)
  (LensFor implicationL)
  (LensFor converseL)
  (LensFor descriptionL)
  (LensFor refsL)
  = getLenses

type Theorem = Theorem' Identity

deriving instance Show Theorem
deriving instance Eq   Theorem

instance ToJSON TheoremId where
  toJSON = Id.toJSON 'T'

instance FromJSON TheoremId where
  parseJSON = Id.parseJSON "TI"

instance Id.Encodable Theorem where
  prefix = 'T'

if' :: Theorem -> Formula PropertyId
if' = Implication.antecedent . implication

then' :: Theorem -> Formula PropertyId
then' = Implication.consequent . implication

properties :: Theorem -> Set PropertyId
properties = Implication.properties . implication
