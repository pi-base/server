module Data.Space
  ( fetch
  , find
  , pending
  , put
  ) where

import Core
import Data        (findParsed, makeId, required, updateBranch)
import Data.Git    (writePages)

import qualified Data.Parse as Parse
import qualified Page
import Page.Space (page)

find :: MonadStore m => Branch -> SpaceId -> m (Maybe Space)
find = Data.findParsed Parse.space

fetch :: (MonadStore m, MonadThrow m) => Branch -> SpaceId -> m Space
fetch sha _id = find sha _id >>= Data.required "Space" (unId _id)

pending :: SpaceId
pending = Id ""

put :: (MonadStore m, MonadThrow m, MonadLogger m) 
    => Branch -> CommitMeta -> Space -> m (Space, Sha)
put branch meta space' = do
  space <- assignId space'
  updateBranch branch meta $ \_ -> do
    writePages [Page.write page space]
    return space

assignId :: MonadIO m => Space -> m Space
assignId p = if spaceId p == pending
  then do
    _id <- makeId "s"
    return $ p { spaceId = _id }
  else return p

