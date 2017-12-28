module Data.Space
  ( describe
  , fetch
  , pending
  , put
  ) where

import Core hiding (find)
import Data        (makeId)
import Data.Git    (writePages, updateRef)

import qualified Page
import Page.Space (page)

-- TODO: have descriptions be lazy-loaded? Not stored in the data types? IORefs?
describe :: (MonadStore m, MonadThrow m) => Maybe Committish -> Space -> m Text
describe mc s = spaceDescription <$> case mc of
  Nothing -> return s
  Just c  -> fetch c $ spaceId s

find :: MonadStore m => Committish -> SpaceId -> m (Maybe Space)
find = error "find"

fetch :: (MonadStore m, MonadThrow m) => Committish -> SpaceId -> m Space
fetch sha _id = find sha _id >>= maybe (throwM . NotFound $ unId _id) return

pending :: SpaceId
pending = Id ""

put :: (MonadStore m, MonadThrow m) => Ref -> CommitMeta -> Space -> m (Version, Space)
put ref meta space' = do
  space <- assignId space'
  version <- updateRef ref meta $ writePages [Page.write page space]
  return (version, space)

assignId :: MonadIO m => Space -> m Space
assignId p = if spaceId p == pending
  then do
    _id <- makeId "s"
    return $ p { spaceId = _id }
  else return p

