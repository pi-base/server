module Data.Space
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
import Page.Space (page)

find :: Git m => Branch -> SpaceId -> m (Maybe Space)
find = Data.findParsed Parse.space

fetch :: Git m => Branch -> SpaceId -> m Space
fetch sha _id = find sha _id >>= Data.required "Space" (unId _id)

put :: (Git m, MonadLogger m)
    => Branch -> CommitMeta -> Space -> m (Space, Sha)
put branch meta space' = do
  space <- assignId space'
  updateBranch branch meta $ \_ -> do
    writePages [Page.write page space]
    return space
