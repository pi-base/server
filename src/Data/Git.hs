module Data.Git
  ( Store
  , MonadStore(..)
  , eachBlob
  , getDir
  , mkStore
  , storeCached
  , modifyGitRef
  , useRepo
  , writeContents
  , ensureUserBranch
  ) where

import Core
import Model (User(..))
import Viewer

import Control.Concurrent.MVar.Lifted (modifyMVar)
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.Tagged
import Data.Time.LocalTime (getZonedTime)
import Git
import Git.Libgit2 (LgRepo, openLgRepository, runLgRepository)

class (MonadBaseControl IO m, MonadIO m, MonadMask m) => MonadStore m where
  getStore :: m Store

instance (MonadStore m) => MonadStore (ReaderT LgRepo m) where
  getStore = lift getStore

-- TODO: enforce that only one thread gets to write to a branch at a time
data Store = Store
  { storeRepo  :: LgRepo
  , storeCache :: MVar (Maybe Viewer)
  }

mkStore :: FilePath -> IO Store
mkStore path = do
  let opts = RepositoryOptions
        { repoPath       = path
        , repoWorkingDir = Nothing
        , repoIsBare     = False
        , repoAutoCreate = False
        }
  repo  <- openLgRepository opts
  cache <- newMVar Nothing
  return $ Store repo cache

storeCached :: MonadStore m
            => m (Either a Viewer)
            -> m (Either a Viewer)
storeCached f = do
  Store{..} <- getStore
  modifyMVar storeCache $ \mev -> case mev of
    Just viewer -> return $ (Just viewer, Right viewer)
    _ -> f >>= \case
      Left err     -> return $ (Nothing, Left err)
      Right viewer -> return $ (Just viewer, Right viewer)

useRepo :: MonadStore m
        => ReaderT LgRepo m a
        -> m a
useRepo handler = do
  Store{..} <- getStore
  runLgRepository storeRepo handler

eachBlob :: (MonadGit r m)
         => Tree r
         -> TreeFilePath
         -> m [Record]
eachBlob tree path = getDir tree path >>= \case
  Left _err -> return []
  Right dir -> do
    entries <- listTreeEntries dir
    foldM step [] entries

  where
    step :: MonadGit r m => [Record] -> (TreeFilePath, TreeEntry r) -> m [Record]
    step results (filepath, entry) = case entry of
      BlobEntry _id _ -> do
        blob   <- catBlobUtf8 _id
        return $ (filepath, blob) : results
      _ -> return results

getDir :: (MonadGit r m) => Tree r -> TreeFilePath -> m (Either Error (Tree r))
getDir tree path = treeEntry tree path >>= \case
  Just (TreeEntry _id) -> lookupTree _id >>= return . Right
  _ -> return . Left $ NotATree path

ensureUserBranch :: MonadStore m => User -> m Text
ensureUserBranch User{..} = useRepo $ do
  let branch = "users/" <> userName
      branchRef = "refs/heads/" <> branch
  existing <- resolveReference branchRef
  unless (isJust existing) $ do
    lookupReference "refs/heads/master" >>= \case
      Nothing -> error "Could not resolve master; is the repo configured?"
      Just master -> createReference branchRef master
  return branch

modifyGitRef :: MonadStore m
             => User
             -> Text
             -> CommitMessage
             -> TreeT LgRepo (ReaderT LgRepo m) a
             -> m (Either String (Commit LgRepo))
modifyGitRef user name message updates = useRepo $ do
  let refname = "refs/heads/" <> name
  resolveReference refname >>= \case
    Nothing  -> return . Left $ "Could not resolve ref: " ++ show refname
    Just ref -> do
      parent <- lookupCommit $ Tagged ref
      tree <- lookupTree $ commitTree parent
      (_, newTree) <- withTree tree updates
      (author, committer) <- getSignatures user
      Right <$> createCommit [commitOid parent] newTree author committer message (Just refname)

getSignatures :: MonadIO m => User -> m (Signature, Signature)
getSignatures User{..} = do
  time <- liftIO getZonedTime
  let
    author = defaultSignature
      { signatureEmail = userEmail
      , signatureName  = userName
      , signatureWhen  = time
      }
    committer = defaultSignature
      { signatureEmail = "system@pi-base.org"
      , signatureName  = "Pi-Base"
      , signatureWhen  = time
      }
  return (author, committer)

writeContents :: MonadStore m
              => User
              -> CommitMessage
              -> [(TreeFilePath, Text)]
              -> m (Either String (Commit LgRepo))
writeContents user message files = do
  branch <- ensureUserBranch user

  traceM "Modifying ref"
  modifyGitRef user branch message $ do
    forM_ files $ \(path, contents) -> do
      traceM $ "Writing to " ++ show path
      traceM "Making commit"
      (lift $ createBlobUtf8 contents) >>= putBlob path
