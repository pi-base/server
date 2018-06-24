{-# LANGUAGE TemplateHaskell #-}
module Data.Store
  ( Store
  , currentLoader
  , fetchBranches
  , getStoreBaseVersion
  , initializeStore
  , initializeDownstream
  , loaderAt
  , pushBranches
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
import           System.Directory    (doesDirectoryExist)
import           System.Process      (callCommand)
import           UnliftIO

initializeStore :: (MonadIO m, MonadUnliftIO m, MonadMask m, MonadLogger m)
                => RepoSettings -> m Store
initializeStore RepoSettings{..} = do
  repo <- do
    $(logInfo) $ "Initializing repository at " ++ tshow rsPath
    exists <- liftIO $ doesDirectoryExist rsPath
    unless exists $ do
      {- 
        Clone initial repository along with all remote branches, from https://stackoverflow.com/questions/67699

        $ git clone --mirror <url> <path>
        $ cd path
        $ git config --bool core.bare false
        $ git checkout master

        We probably don't want to actually --mirror here because we don't want remote PR
        branches or anything to cause errors when we do a push
      -}
      run "Cloning initial repository" $ 
        "git clone --mirror " <> rsUpstream <> " " <> T.pack rsPath
      run "Configuring remotes" $
           "cd " <> T.pack rsPath 
        <> " && git remote add downstream " <> rsDownstream 
        <> " && git remote add upstream " <> rsUpstream
    liftIO $ openLgRepository $ RepositoryOptions
      { repoPath       = rsPath
      , repoWorkingDir = Nothing
      , repoIsBare     = True
      , repoAutoCreate = False
      }

  (Just commit) <- runLgRepository repo $ 
    resolveCommittish $ CommitRef $ Ref rsDefaultBranch
  loader <- mkLoader commit >>= newMVar

  return Store 
    { storeRepo       = repo
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
  unless exists $
    run "Initialize downstream" $
      "git clone --mirror " <> T.pack rsPath <> " " <> rsDownstream

fetchBranches :: (MonadStore m, MonadLogger m) => m ()
fetchBranches = do
  repoPath <- storeRepoPath <$> getStore
  run "Pulling updates for repo" $ 
    "cd " <> T.pack repoPath <> " && git fetch upstream"

pushBranches :: (MonadStore m, MonadLogger m) => m ()
pushBranches = do
  repoPath <- storeRepoPath <$> getStore
  -- FIXME: replace w/ shelly, check for errors, capture / handle output
  run "Pushing user branches" $ 
    "cd " <> T.pack repoPath <> " && git push --mirror downstream >/dev/null 2>&1"

run :: (MonadLogger m, MonadIO m) => Text -> Text -> m ()
run desc cmd = do
  $(logInfo) desc
  $(logDebug) cmd
  liftIO . callCommand $ T.unpack cmd

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
