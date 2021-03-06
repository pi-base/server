module Data.Property
  ( fetch
  , find
  , put
  ) where

import Core hiding (find, put)

import Data        (findParsed, required, updateBranch)
import Data.Id     (assignId)
import Data.Git    (writePages)

import qualified Data.Parse as Parse
import qualified Page
import Page.Property (page)

find :: Git m => Branch -> PropertyId -> m (Maybe Property)
find = Data.findParsed Parse.property

fetch :: Git m => Branch -> PropertyId -> m Property
fetch sha _id =
  find sha _id >>= Data.required "Property" (unId _id)

put :: (Git m, MonadLogger m)
    => Branch -> CommitMeta -> Property -> m (Property, Sha)
put branch meta prop' = do
  prop <- assignId prop'
  updateBranch branch meta $ \_ -> do
    writePages [Page.write page prop]
    return prop

