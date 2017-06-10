module Data.Space
  ( describe
  , find
  , fetch
  , pending
  , put
  ) where

import Core hiding (find)
import Data        (makeId)
import Data.Git    (useRepo, writePages, updateRef)
import qualified Data.Parse
import qualified Page.Space

-- TODO: have descriptions be lazy-loaded? Not stored in the data types? IORefs?
describe :: (MonadStore m, MonadThrow m) => Maybe Committish -> Space -> m Text
describe mc s = spaceDescription <$> case mc of
  Nothing -> return s
  Just c  -> fetch c $ spaceId s

find :: MonadStore m => Committish -> SpaceId -> m (Maybe Space)
find = Data.Parse.findSpace

fetch :: (MonadStore m, MonadThrow m) => Committish -> SpaceId -> m Space
fetch sha _id = find sha _id >>= maybe (throwM . NotFound $ unSpaceId _id) return

pending :: SpaceId
pending = SpaceId ""

put :: (MonadStore m, MonadThrow m) => Ref -> CommitMeta -> Space -> m (Version, Space)
put ref meta space' = do
  space <- assignId space'
  useRepo $ do
    version <- updateRef ref meta $ writePages [Page.Space.write space]
    return (version, space)

assignId :: MonadIO m => Space -> m Space
assignId p = if spaceId p == pending
  then do
    _id <- makeId SpaceId "s"
    return $ p { spaceId = _id }
  else return p

