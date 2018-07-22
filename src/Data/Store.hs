{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
module Data.Store
  ( Store
  , currentLoader
  , fetchBranch
  , fetchBranches
  , getStoreBaseVersion
  , initializeDownstream
  , initializeStore
  , loaderAt
  , pushBranch
  , storeAutoSync
  , storeBaseBranch
  , storeLoader
  , storeRepo
  ) where

import Import.NoFoundation hiding (head, newMVar, readMVar, modifyMVar)

import           Class               (MonadStore(..))
import           Control.Monad.Catch (MonadMask)
import           Data.Git            as Git
import qualified Data.Text           as T
import           Data.Loader         as Loader
import           Git
import           Git.Libgit2         (openLgRepository, runLgRepository)
import           Types
import           Types.Store
import           Shelly
import           System.Directory    (doesDirectoryExist)
import           UnliftIO

default (T.Text)

initializeStore :: (MonadIO m, MonadUnliftIO m, MonadMask m, MonadLogger m)
                => RepoSettings -> m Store
initializeStore RepoSettings{..} = do
  lgRepo <- do
    $(logInfo) $ "Initializing repository at " ++ tshow rsPath
    let repoPath = fromText $ T.pack rsPath
    exists <- liftIO $ doesDirectoryExist rsPath
    unless exists $ 
      shell "Initializing repository" $ do
        void $ git "clone" "--mirror" rsUpstream repoPath
        cd repoPath
        void $ git "remote" "add" "downstream" rsDownstream
        git "remote" "add" "upstream" rsDownstream
    liftIO $ openLgRepository $ RepositoryOptions
      { repoPath       = rsPath
      , repoWorkingDir = Nothing
      , repoIsBare     = True
      , repoAutoCreate = False
      }

  (Just commit) <- runLgRepository lgRepo $ 
    resolveCommittish $ CommitRef $ Ref rsDefaultBranch
  loader <- mkLoader commit >>= newMVar

  return Store 
    { storeRepo       = lgRepo
    , storeLoader     = loader
    , storeRepoPath   = rsPath
    , storeBaseBranch = rsDefaultBranch
    , storeAutoSync   = rsAutoPush
    , storeUpstream   = rsUpstream
    , storeDownstream = rsDownstream
    }

-- N.B. This tacitly assumes that downstream is a local filepath
-- This is mostly intended for testing
initializeDownstream :: (MonadLogger m, MonadIO m) => RepoSettings -> m ()
initializeDownstream RepoSettings{..} = do
  exists <- liftIO $ doesDirectoryExist $ T.unpack rsDownstream
  let repoPath = fromText $ T.pack rsPath
  unless exists $
    shell "Initialize downstream" $ do
      git "clone" "--mirror" repoPath rsDownstream

fetchBranches :: (MonadStore m, MonadLogger m) => m ()
fetchBranches = repo "Fetching updates for repo" $ do
  git "fetch"

pushBranch :: (MonadStore m, MonadLogger m) => Branch -> m ()
pushBranch Branch{..} = repo ("Pushing " <> branchName) $ do
  git "push" "downstream" branchName

fetchBranch :: (MonadStore m, MonadLogger m) => Branch -> m ()
fetchBranch Branch{..} =  repo ("Fetching " <> branchName) $ do
  git "fetch" "origin" branchName

repo :: (MonadLogger m, MonadStore m) => Text -> Sh a -> m ()
repo msg handler = do
  repoPath <- storeRepoPath <$> getStore
  shell msg $ do
    cd $ fromText $ T.pack repoPath
    handler

shell :: (MonadLogger m, MonadIO m) => Text -> Sh a -> m ()
shell _ = void . shelly . silently

git :: ShellCmd result => Shelly.FilePath -> result
git = cmd "git"

getStoreBaseVersion :: MonadStore m => m Version
getStoreBaseVersion = do
  store  <- getStore
  loader <- readMVar $ storeLoader store
  return $ Loader.version loader

loaderAt :: MonadStore m => Text -> m Loader
loaderAt label = do
  commit    <- Git.commitFromLabel $ Just label
  loaderVar <- storeLoader <$> getStore
  loader    <- readMVar loaderVar
  if (commitSha commit) == (commitSha $ Loader.commit loader)
    then return loader
    else Loader.mkLoader commit

currentLoader :: MonadStore m => m Loader
currentLoader = do
  loaderVar <- storeLoader <$> getStore
  modifyMVar loaderVar $ \loader -> do
    head <- Git.baseCommit
    if (commitSha head) == (commitSha $ Loader.commit loader)
      then return (loader, loader)
      else do
        newLoader <- Loader.mkLoader head
        return (newLoader, newLoader)
