module Data.Space
  ( describe
  , find
  , fetch
  , pending
  , put
  ) where

import Core hiding (find)

describe :: (MonadStore m, MonadThrow m) => Committish -> SpaceId -> m Text
describe = error "describeSpace"

find :: MonadStore m => Committish -> SpaceId -> m (Maybe Space)
find = error "findSpace"

fetch :: (MonadStore m, MonadThrow m) => Committish -> SpaceId -> m Space
fetch = error "fetchSpace"

pending :: SpaceId
pending = SpaceId ""

put :: (MonadStore m, MonadThrow m) => Ref -> CommitMeta -> Space -> m (Version, Space)
put = error "putSpace"
