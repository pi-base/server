module Data.Trait
  ( put
  ) where

import           Core
import           Data  (updateView, viewDeductions)
import qualified Logic as L

put :: (MonadStore m, MonadThrow m)
    => Ref
    -> CommitMeta
    -> Trait SpaceId PropertyId
    -> m (Either [Error] View)
put ref meta trait = do
  result <- updateView ref meta $ \loader -> do
    (lift $ L.runLogicT loader $ L.assertTrait trait) >>= \case
      Left err -> return . Left $ LogicError err
      Right updates -> lift $ viewDeductions loader updates
  case result of
    Left err -> return $ Left [ err ]
    Right v  -> return $ Right v
