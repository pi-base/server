module Data.Theorem
  ( describe
  , find
  , fetch
  , pending
  , put
  ) where

import Core hiding (find)

describe :: (MonadStore m, MonadThrow m) => Maybe Committish -> Theorem p -> m Text
describe = error "describeTheorem"

find :: MonadStore m => Committish -> TheoremId -> m (Maybe (Theorem Property))
find = undefined

fetch :: (MonadStore m, MonadThrow m) => Committish -> TheoremId -> m (Theorem Property)
fetch = error "fetchTheorem"

pending :: TheoremId
pending = TheoremId ""

put :: (MonadStore m, MonadThrow m) => Ref -> CommitMeta -> Theorem Property -> m (Version, Theorem Property)
put = error "putTheorem"
