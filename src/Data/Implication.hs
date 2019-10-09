module Data.Implication
  ( Implication(..)
  , (~>)
  , antecedent
  , consequent
  , contrapositive
  , converse
  , negative
  , properties
  , name
  ) where

import Import

import qualified Data.Formula as Formula
import           Data.Formula (Formula)
import qualified Data.Set     as Set

data Implication p = Implication (Formula p) (Formula p)
  deriving (Generic, Eq, Functor, Foldable, Show, Traversable)

(~>) :: Formula p -> Formula p -> Implication p
(~>) = Implication
infixl 3 ~>

antecedent :: Implication p -> Formula p
antecedent (Implication a _) = a

consequent :: Implication p -> Formula p
consequent (Implication _ c) = c

contrapositive :: Implication p -> Implication p
contrapositive (Implication ant con) = Implication (Formula.negate con) (Formula.negate ant)

converse :: Implication p -> Implication p
converse (Implication ant con) = Implication con ant

negative :: Implication p -> Implication p
negative (Implication ant con) = Implication (Formula.negate ant) (Formula.negate con)

properties :: Ord p => Implication p -> Set p
properties (Implication a c) = Formula.properties a `Set.union` Formula.properties c

name :: Implication Text -> Text
name (Implication a c) = Formula.format identity a <> " => " <> Formula.format identity c
