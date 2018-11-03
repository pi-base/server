{-# LANGUAGE TemplateHaskell #-}
module Data.Branch.Merge
  ( Merge(..)
  , merge
  ) where

import Protolude hiding (from)
import Core

import           Conduit
import           Data.Git    as Git
import qualified Data.Map    as M
import qualified Data.Set    as S
import           Git         as Git

import           Data.Branch      as Branch
import           Data.Branch.Move (moveMaps)
import qualified Data.Id          as Id
import           Data.Parse       (propertyIds, spaceIds, theoremIds)

data Merge a = Merge
  { from :: a
  , into :: a
  }

merge :: (Git m, DB m, MonadLogger m) => Merge Branch -> CommitMeta -> m Sha
merge Merge{..} meta = storeLocked $ do
  fetch into

  Branch.claimUserBranches

  (fromCommit, fromTree) <- branchTree from
  (intoCommit, intoTree) <- branchTree into
  let branchMerge = Merge { from = fromTree, into = intoTree }

  propMap    <- buildIdMap propertyIds branchMerge
  spaceMap   <- buildIdMap spaceIds    branchMerge
  theoremMap <- buildIdMap theoremIds  branchMerge

  -- TODO: this could definitely be more efficient
  -- * stream entries one at a time
  -- * check if there's a better way to skip writing
  entries <- sourceToList $ sourceTreeEntries fromTree
  (_, merged) <- withTree intoTree $ do
    forM_ entries $ uncurry mergeBlob
    moveMaps spaceMap propMap theoremMap
  commitBranch (branchName into) merged meta [fromCommit, intoCommit]

mergeBlob :: (MonadGit LgRepo m, MonadLogger m) => TreeFilePath -> TreeEntry LgRepo -> TreeT LgRepo m ()
mergeBlob path (BlobEntry oid kind) = getEntry path >>= \case
  Just (BlobEntry oid' _) ->
    unless (oid == oid') $ do
      $(logDebug) $ "Copying " <> show path
      putBlob' path oid kind
  _ -> return ()
mergeBlob _ _ = return ()

buildIdMap :: (Id.Identifiable a, MonadIO m)
           => (Tree LgRepo -> ConduitT () (Id a) m ())
           -> Merge (Tree LgRepo)
           -> m (Map (Id a) (Id a))
buildIdMap parser Merge{..} = do
  fromIds <- sourceToList $ parser from
  baseIds <- sourceToList $ parser into

  let
    ids :: S.Set Int
    ids = foldr' (\id -> maybe identity S.insert (Id.toInt id)) S.empty baseIds

    addId :: (Id.Identifiable a, Ord k, Show k) => (M.Map k (Id a), Int) -> k -> (M.Map k (Id a), Int)
    addId (m, cursor) id = if S.member cursor ids
      then addId (m, succ cursor) id
      else (M.insert id (Id.fromInt cursor) m, succ cursor)

    processId :: Id.Identifiable a => Id a -> (M.Map (Id a) (Id a), Int) -> (M.Map (Id a) (Id a), Int)
    processId fromId (m, cursor) = case Id.toInt fromId of
      Nothing -> addId (m, cursor) fromId
      Just i -> if S.member i ids
        then (m, cursor)
        else (M.insert fromId fromId m, cursor)

    (idMap, _) = foldr' processId (M.empty, 1) $ reverse $ sort fromIds

  return idMap

branchTree :: Git m => Branch -> m (Commit LgRepo, Tree LgRepo)
branchTree branch = do
  c <- fetchRefHead $ ref branch
  t <- lookupTree $ commitTree c
  return (c, t)