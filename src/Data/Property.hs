module Data.Property
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
import qualified Page.Property

describe :: (MonadStore m, MonadThrow m) => Maybe Committish -> Property -> m Text
describe mc p = propertyDescription <$> case mc of
  Nothing -> return p
  Just c  -> fetch c $ propertyId p

find :: MonadStore m => Committish -> PropertyId -> m (Maybe Property)
find = Data.Parse.findProperty

fetch :: (MonadStore m, MonadThrow m) => Committish -> PropertyId -> m Property
fetch sha _id = find sha _id >>= maybe (throwM . NotFound $ unPropertyId _id) return

pending :: PropertyId
pending = PropertyId ""

put :: (MonadStore m, MonadThrow m)
    => Ref -> CommitMeta -> Property -> m (Version, Property)
put ref meta prop' = do
  prop <- assignId prop'
  useRepo $ do
    version <- updateRef ref meta $ writePages [Page.Property.write prop]
    return (version, prop)

assignId :: MonadIO m => Property -> m Property
assignId p = if propertyId p == pending
  then do
    _id <- makeId PropertyId "p"
    return $ p { propertyId = _id }
  else return p

