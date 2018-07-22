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
import qualified Data.Id     as Id
import           Data.Parse  (propertyIds, spaceIds, theoremIds)
import           Types

data Merge a = Merge
  { from :: a
  , into :: a
  }

merge :: (MonadStore m, MonadLogger m) => Merge Branch -> CommitMeta -> m Sha
merge patch@Merge{..} meta = do
  fetch into

  fromCommit <- fetchRefHead $ ref from
  intoCommit <- fetchRefHead $ ref into
  let commitMerge = Merge { from = fromCommit, into = intoCommit }

  propMap <- buildIdMap propertyIds commitMerge
  traceM "propMap"
  traceM $ show propMap

  spaceMap <- buildIdMap spaceIds commitMerge
  traceM "spaceMap"
  traceM $ show spaceMap

  theoremMap <- buildIdMap theoremIds commitMerge
  traceM "theoremMap"
  traceM $ show theoremMap

  -- Git.merge commitMerge meta $ do
  --   renumberSpaces spaceMap
  --   renumberProperties propertyMap
  --   renumberTheorems theoremMap

  Data.Branch.headSha into

buildIdMap :: (Id.Identifiable a, MonadIO m) 
           => (Commit LgRepo -> Source m (Id a))
           -> Merge (Commit LgRepo)
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
