module Data.Theorem
  ( describe
  , find
  , fetch
  , pending
  , put
  ) where

import Core hiding (find)
import qualified Data.Parse

describe :: (MonadStore m, MonadThrow m) => Maybe Committish -> Theorem p -> m Text
describe mc t = case mc of
  Nothing -> return $ theoremDescription t
  Just c  -> fmap theoremDescription . fetch c $ theoremId t

find :: MonadStore m => Committish -> TheoremId -> m (Maybe (Theorem Property))
find = Data.Parse.findTheorem

fetch :: (MonadStore m, MonadThrow m) => Committish -> TheoremId -> m (Theorem Property)
fetch sha _id = find sha _id >>= maybe (throwM . NotFound $ unTheoremId _id) return

pending :: TheoremId
pending = TheoremId ""

put :: (MonadStore m, MonadThrow m) => Ref -> CommitMeta -> Theorem Property -> m (Version, Theorem Property)
put = error "putTheorem"
