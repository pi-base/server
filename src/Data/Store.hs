{-# LANGUAGE TemplateHaskell #-}
module Data.Store
  ( Store
  , currentLoader
  , fetchBranches
  , getStoreBaseVersion
  , initializeStore
  , loaderAt
  , pushBranches
  , storeAutoSync
  , storeBaseBranch
  , storeLoader
  , storeRepo
  ) where

import Import.NoFoundation hiding (head)

import           Class            (MonadStore(..))
import           Data.Git         as Git
import qualified Data.Text        as T
import           Data.Loader      as Loader
import           Git
import           Git.Libgit2      (LgRepo, openLgRepository, runLgRepository)
import           Types
import           Types.Store
import           System.Directory (doesDirectoryExist)
import           System.Process   (callCommand)

initializeRepo :: (MonadIO m, MonadLogger m) => FilePath -> m LgRepo
initializeRepo path = do
  $(logInfo) $ "Initializing repository at " ++ tshow path
  exists <- liftIO $ doesDirectoryExist path
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
      "git clone --bare git@github.com:pi-base/data.git " <> T.pack path <> "/.git"
  liftIO $ openLgRepository $ RepositoryOptions
    { repoPath       = path
    , repoWorkingDir = Nothing
    , repoIsBare     = True
    , repoAutoCreate = False
    }

initializeStore :: (MonadIO m, MonadBase IO m, MonadBaseControl IO m, MonadMask m, MonadLogger m)
                => FilePath -> BranchName -> Bool -> m Store
initializeStore storeRepoPath storeBaseBranch storeAutoSync = do
  storeRepo     <- initializeRepo storeRepoPath
  (Just commit) <- runLgRepository storeRepo $ resolveCommittish $ CommitRef $ Ref storeBaseBranch
  loader        <- mkLoader commit
  storeLoader   <- newMVar loader
  return Store{..}

fetchBranches :: (MonadStore m, MonadLogger m) => m ()
fetchBranches = do
  repoPath <- storeRepoPath <$> getStore
  run "Pulling updates for repo" $ 
    "cd " <> T.pack repoPath <> " && git fetch origin"

pushBranches :: (MonadStore m, MonadLogger m) => m ()
pushBranches = do
  repoPath <- storeRepoPath <$> getStore
  run "Pushing user branches" $ 
    "cd " <> T.pack repoPath <> " && git push origin users/*"

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
  loader    <- readMVar loaderVar
  head      <- Git.baseCommit 
  if (commitSha head) == (commitSha $ Loader.commit loader)
    then return loader
    else do
      newLoader <- Loader.mkLoader head
      putMVar loaderVar newLoader
      return newLoader
