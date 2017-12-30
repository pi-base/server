module Data.Property
  ( fetch
  , pending
  , put
  ) where

import Core hiding (find)
import Data        (makeId)
import Data.Git    (writePages, updateBranch)

import qualified Page
import Page.Property (page)

find :: MonadStore m => Branch -> PropertyId -> m (Maybe Property)
find = error "find"

fetch :: (MonadStore m, MonadThrow m) => Branch -> PropertyId -> m Property
fetch sha _id = find sha _id >>= maybe (throwM . NotFound $ unId _id) return

pending :: PropertyId
pending = Id ""

put :: (MonadStore m, MonadThrow m)
    => Branch -> CommitMeta -> Property -> m (Property, Sha)
put branch meta prop' = do
  prop <- assignId prop'
  updateBranch branch meta $ \_ -> do
    writePages [Page.write page prop]
    return prop

assignId :: MonadIO m => Property -> m Property
assignId p = if propertyId p == pending
  then do
    _id <- makeId "p"
    return $ p { propertyId = _id }
  else return p

