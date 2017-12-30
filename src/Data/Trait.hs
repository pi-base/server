module Data.Trait
  ( put
  ) where

import           Core
import           Data  (updateView, viewDeductions)
import qualified Logic as L

put :: (MonadStore m, MonadThrow m)
    => Branch
    -> CommitMeta
    -> Trait SpaceId PropertyId
    -> m View
put branch meta trait = updateView branch meta $ \loader -> do
  result <- L.runLogicT loader $ L.assertTrait trait
  case result of
    Left      err -> throw $ LogicError err
    Right updates -> viewDeductions loader $ updates