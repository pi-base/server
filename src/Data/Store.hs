{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
module Data.Store
  ( module Store
  , build
  , currentLoader
  , fetchBranch
  , fetchBranches
  , getBaseVersion
  , getDefaultBranch
  , loaderAt
  , pushBranch
  ) where

import Core hiding (head, newMVar, readMVar, modifyMVar)

import           Control.Lens        (view)
import           Control.Monad.Catch (MonadMask)
import           Data.Git            as Git
import qualified Data.Text           as T
import           Data.Store.Types    as Store
import           Data.Loader         as Loader
import           Git
import           Git.Libgit2         (openLgRepository, runLgRepository)
import           Shelly
import           System.Directory    (doesDirectoryExist)
import           UnliftIO

default (T.Text)

build :: (MonadIO m, MonadUnliftIO m, MonadMask m, MonadLogger m)
      => Store.Settings -> m (Either Text Store)
build settings@Store.Settings{..} = do
  _storeRepo <- do
    $(logInfo) $ "Initializing repository at " <> T.pack _repoPath
    exists <- liftIO $ doesDirectoryExist _repoPath
    unless exists $
      shell "Initializing repository" $
        void $ git "clone" "--mirror" _upstream $ fromText $ T.pack _repoPath
    liftIO $ openLgRepository $ RepositoryOptions
      { repoPath       = _repoPath
      , repoWorkingDir = Nothing
      , repoIsBare     = True
      , repoAutoCreate = False
      }

  mcommit <- runLgRepository _storeRepo $
    resolveCommittish $ CommitRef $ Ref $ settings ^. defaultBranch
  case mcommit of
    Just commit -> do
      loader <- mkLoader commit >>= newMVar
      mutex <- newMVar ()
      return $ Right $ Store
        { _storeSettings  = settings
        , _storeRepo      = _storeRepo
        , _storeLoader    = loader
        , _storeWriteLock = mutex
        }
    Nothing -> return $ Left $ "failed to find default branch " <> settings ^. defaultBranch

fetchBranches :: (Git m, MonadLogger m) => m ()
fetchBranches = exec "Fetching updates for repo" $ do
  git "fetch"

pushBranch :: (Git m, MonadLogger m) => Branch -> m ()
pushBranch Branch{..} = exec ("Pushing " <> branchName) $ do
  git "push" "origin" -- branchName -- we assume this is a --mirror just push everything

fetchBranch :: (Git m, MonadLogger m) => Branch -> m ()
fetchBranch Branch{..} =  exec ("Fetching " <> branchName) $ do
  git "fetch" "origin" branchName

exec :: (MonadLogger m, Git m) => Text -> Sh a -> m ()
exec msg handler = do
  dir <- view (storeSettings . Store.repoPath) <$> getStore
  shell msg $ do
    cd $ fromText $ T.pack $ dir
    handler

shell :: (MonadLogger m, MonadIO m) => Text -> Sh a -> m ()
shell _ = void . shelly . silently

git :: ShellCmd result => Shelly.FilePath -> result
git = cmd "git"

getBaseVersion :: Git m => m Version
getBaseVersion = do
  load <- readMVar =<< view storeLoader <$> getStore
  return $ Loader.version load

getDefaultBranch :: Git m => m BranchName
getDefaultBranch = view (storeSettings . defaultBranch) <$> getStore

loaderAt :: Git m => Text -> m Loader
loaderAt label = do
  commit <- Git.commitFromLabel $ Just label
  load   <- readMVar =<< view storeLoader <$> getStore
  if (commitSha commit) == (commitSha $ Loader.commit load)
    then return load
    else Loader.mkLoader commit

currentLoader :: Git m => m Loader
currentLoader = do
  loaderVar <- view storeLoader <$> getStore
  modifyMVar loaderVar $ \l -> do
    head <- Git.baseCommit
    if (commitSha head) == (commitSha $ Loader.commit l)
      then return (l, l)
      else do
        newLoader <- Loader.mkLoader head
        return (newLoader, newLoader)
