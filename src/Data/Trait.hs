module Data.Trait
  ( put
  ) where

import           Core
import           Data
import qualified Data.Parse
import qualified Logic      as L

put :: (MonadStore m, MonadThrow m)
    => Ref
    -> CommitMeta
    -> Trait SpaceId PropertyId
    -> m (Either [Error] View)
put ref meta trait = do
  result <- updateView ref meta $ do
    cloader <- Data.Parse.cloader
    let loader = bridgeLoader cloader
    (lift $ L.runLogicT loader $ L.assertTrait trait) >>= \case
      Left err -> return . Left $ LogicError err
      Right updates -> lift $ viewDeductions cloader updates
  case result of
    Left err -> return $ Left [ err ]
    Right v  -> return $ Right v
