module Data.Theorem
  ( describe
  , fetch
  , pending
  , put
  ) where

import           Core  hiding (find)
import           Data  (makeId, updateView, viewDeductions)
import qualified Logic as L

describe :: (MonadStore m, MonadThrow m) => Maybe Committish -> Theorem p -> m Text
describe mc t = case mc of
  Nothing -> return $ theoremDescription t
  Just c  -> fmap theoremDescription . fetch c $ theoremId t

find :: MonadStore m => Committish -> TheoremId -> m (Maybe (Theorem Property))
find = error "find"

fetch :: (MonadStore m, MonadThrow m) => Committish -> TheoremId -> m (Theorem Property)
fetch sha _id = find sha _id >>= maybe (throwM . NotFound $ unId _id) return

pending :: TheoremId
pending = Id ""

put :: (MonadStore m, MonadThrow m) => Ref -> CommitMeta -> Theorem PropertyId -> m (Either [Error] View)
put ref meta theorem' = do
  theorem <- assignId theorem'

  result <- updateView ref meta $ \loader -> do
    (lift $ L.runLogicT loader $ L.assertTheorem theorem) >>= \case
      Left err -> return . Left $ LogicError err
      Right updates -> lift $ viewDeductions loader updates
  case result of
    Left err -> return $ Left [ err ]
    Right v  -> return $ Right v

assignId :: MonadIO m => Theorem p -> m (Theorem p)
assignId t = if theoremId t == pending
  then do
    _id <- makeId "t"
    return $ t { theoremId = _id }
  else return t
