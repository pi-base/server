module Data.Branch.Merge
  ( Merge(..)
  , merge
  ) where

import Protolude
import Core

import           Conduit     (Source, sourceToList)
import           Data.Git    as Git
import qualified Data.Map    as M
import qualified Data.Set    as S
import           Data.Tagged
import           Git         as Git

import           Data.Branch
import           Data.Branch.Move (moveMaps)
import qualified Data.Id          as Id
import           Data.Parse       (propertyIds, spaceIds, theoremIds)
import           Types

data Merge a = Merge
  { from :: a
  , into :: a
  }

merge :: (MonadStore m, MonadLogger m) => Merge Branch -> CommitMeta -> m Sha
merge patch@Merge{..} meta = do
  fetch into

  fromBranch <- treeBranch from
  intoBranch <- treeBranch into
  let branchMerge = Merge { from = fromBranch, into = intoBranch }

  propMap    <- buildIdMap propertyIds branchMerge
  spaceMap   <- buildIdMap spaceIds    branchMerge
  theoremMap <- buildIdMap theoremIds  branchMerge

  traceM "propMap"
  traceM $ show propMap
  traceM ""
  traceM "spaceMap"
  traceM $ show spaceMap
  traceM ""
  traceM "theoremMap"
  traceM $ show theoremMap
  traceM ""

  -- undefined $ do -- do the commit
  --   moveMaps spaceMap propMap theoremMap
  --   -- and actually do the merge
  undefined

buildIdMap :: (Id.Identifiable a, MonadIO m) 
           => (Tree LgRepo -> Source m (Id a))
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

treeBranch :: MonadStore m => Branch -> m (Tree LgRepo)
treeBranch branch = do
  commit <- fetchRefHead $ ref branch
  lookupTree $ commitTree commit