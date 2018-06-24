module Data.Property
  ( fetch
  , find
  , pending
  , put
  ) where

import Protolude hiding (find, put)

import Core
import Data        (findParsed, makeId, required, updateBranch)
import Data.Git    (writePages)

import qualified Data.Parse as Parse
import qualified Page
import Page.Property (page)

find :: MonadStore m => Branch -> PropertyId -> m (Maybe Property)
find = Data.findParsed Parse.property

fetch :: MonadStore m => Branch -> PropertyId -> m Property
fetch sha _id =
  find sha _id >>= Data.required "Property" (unId _id)

pending :: PropertyId
pending = Id ""

put :: (MonadStore m, MonadLogger m)
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

