module Data.Property
  ( describe
  , fetch
  , pending
  , put
  ) where

import Core hiding (find)
import Data        (makeId)
import Data.Git    (writePages, updateRef)

import qualified Page
import Page.Property (page)

describe :: (MonadStore m, MonadThrow m) => Maybe Committish -> Property -> m Text
describe mc p = propertyDescription <$> case mc of
  Nothing -> return p
  Just c  -> fetch c $ propertyId p

find :: MonadStore m => Committish -> PropertyId -> m (Maybe Property)
find = error "find"

fetch :: (MonadStore m, MonadThrow m) => Committish -> PropertyId -> m Property
fetch sha _id = find sha _id >>= maybe (throwM . NotFound $ unId _id) return

pending :: PropertyId
pending = Id ""

put :: (MonadStore m, MonadThrow m)
    => Ref -> CommitMeta -> Property -> m (Version, Property)
put ref meta prop' = do
  prop <- assignId prop'
  version <- updateRef ref meta $ writePages [Page.write page prop]
  return (version, prop)

assignId :: MonadIO m => Property -> m Property
assignId p = if propertyId p == pending
  then do
    _id <- makeId "p"
    return $ p { propertyId = _id }
  else return p

