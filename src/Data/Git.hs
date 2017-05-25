module Data.Git
  ( Store
  , MonadStore(..)
  , eachBlob
  , getDir
  , mkStore
  , storeCached
  , useRepo
  , writeContents
  , ensureUserBranch
  ) where

import Core
import Model (User(..))
import Viewer

-- import Control.Concurrent.MVar (MVar, newMVar, newEmptyMVar, readMVar,
--                                 tryTakeMVar, tryPutMVar)
import Control.Concurrent.MVar.Lifted (modifyMVar)
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.Tagged
import Git
import Git.Libgit2 (LgRepo, openLgRepository, runLgRepository)

class (MonadBaseControl IO m, MonadIO m, MonadMask m) => MonadStore m where
  getStore :: m Store

instance (MonadStore m) => MonadStore (ReaderT LgRepo m) where
  getStore = lift getStore

data Store = Store
  { storeRepo  :: MVar LgRepo
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
  repo <- openLgRepository opts
  Store <$> newMVar repo <*> newMVar Nothing

storeCached :: MonadStore m
            => Store
            -> (Store -> m (Either a Viewer))
            -> m (Either a Viewer)
storeCached store f = modifyMVar (storeCache store) $ \mev ->
  case mev of
    Just viewer -> return $ (Just viewer, Right viewer)
    _ -> f store >>= \case
      Left err     -> return $ (Nothing, Left err)
      Right viewer -> return $ (Just viewer, Right viewer)

useRepo :: MonadStore m
        => Store
        -> ReaderT LgRepo m a
        -> m a
useRepo store handler = withMVar (storeRepo store) $ \r ->
  runLgRepository r handler

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
getDir tree path = do
  entry <- treeEntry tree path
  case entry of
    Just (TreeEntry _id) -> do
      t <- lookupTree _id
      return $ Right t
    _ -> return . Left $ NotATree path

userBranch :: User -> RefName
userBranch User{..} = "refs/heads/users/" <> userName

ensureUserBranch :: MonadGit r m => User -> m ()
ensureUserBranch user = resolveReference branch >>= \case
  Just _ -> return ()
  Nothing -> do
    lookupReference "refs/heads/master" >>= \case
      Nothing -> error "Could not resolve master; is the repo configured?"
      Just master -> createReference branch master
  where
    branch = userBranch user

{- For reference only
-  Items below here should be cleaned up at some point
------------------------------------------------------
-}

writeContents :: (MonadStore m)
              => User
              -> CommitMessage
              -> [(TreeFilePath, Text)]
              -> ReaderT LgRepo m ()
writeContents user message files = do
  ensureUserBranch user -- TODO: move to registration time

  let refname = userBranch user
  mref <- resolveReference refname
  (parent, tree) <- case mref of
    Nothing  -> error "Could not find user branch"
    Just ref -> do
      parent <- lookupCommit $ Tagged ref
      tree   <- lookupTree $ commitTree parent
      return (parent, tree)

  blobs <- forM files $ \(file, contents) -> do
    _id <- createBlobUtf8 contents
    return (file, _id)

  (_,tid) <- withTree tree $ do
    forM_ blobs $ \(file, _id) -> putEntry file (BlobEntry _id PlainBlob)
  _ <- commit refname parent tid message
  return ()

commit :: MonadGit r m => RefName -> Commit r -> TreeOid r -> CommitMessage -> m (Commit r)
commit refname parent tree message = do
  let sig = defaultSignature
        { signatureName = "James Dabbs"
        , signatureEmail = "jamesdabbs@gmail.com"
        }
  createCommit [commitOid parent] tree sig sig message (Just refname)
