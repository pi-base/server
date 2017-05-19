module Data.Git
  ( Store
  , MonadStore
  , eachBlob
  , getDir
  , mkStore
  , storeCached
  , useRepo
  , writeContents
  ) where

import Core
import Model (User(..))
import Viewer

-- import Control.Concurrent.MVar (MVar, newMVar, newEmptyMVar, readMVar,
--                                 tryTakeMVar, tryPutMVar)
import Control.Concurrent.MVar.Lifted (withMVar)
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.Tagged
import Git
import Git.Libgit2 (LgRepo, openLgRepository, runLgRepository)

class (MonadBaseControl IO m, MonadIO m, MonadMask m) => MonadStore m

instance (MonadStore m) => MonadStore (ReaderT LgRepo m)

data Store = Store
  { storePath  :: FilePath
  , storeRepo  :: MVar LgRepo
  , storeCache :: MVar Viewer
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
  Store
    <$> pure path
    <*> newMVar repo
    <*> newEmptyMVar

storeCached :: MonadIO m
            => Store
            -> (Store -> m (Either a Viewer))
            -> m (Either a Viewer)
storeCached s f = do
  let cache = storeCache s
  mev <- liftIO $ tryTakeMVar cache
  case mev of
    Just viewer -> return $ Right viewer
    _ -> do
      ev <- f s
      case ev of
        Left err -> return $ Left err
        Right v -> do
          liftIO $ tryPutMVar cache v
          return $ Right v

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
eachBlob tree path = do
  edir <- getDir tree path
  case edir of
    Left err -> return []
    Right dir -> do
      entries <- listTreeEntries dir
      foldM step [] entries

   where
     step :: MonadGit r m => [Record] -> (TreeFilePath, TreeEntry r) -> m [Record]
     step results (path, entry) = case entry of
       BlobEntry _id _ -> do
         blob   <- catBlobUtf8 _id
         return $ (path, blob) : results
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
userBranch User{..} = "refs/heads/users/" <> userIdent

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

p = error "p"

printEntries tree = do
  entries <- listTreeEntries tree
  forM_ entries $ \(path, entry) -> do
    let oid = treeEntryToOid entry
    p (path, oid)

findOrCreateTree :: (MonadStore m, MonadGit r m) => RefName -> m (Commit r, Tree r)
findOrCreateTree refname = do
  mref <- resolveReference refname
  ref  <- case mref of
    Just r  -> return r
    Nothing -> do
      lookupReference "master" >>= \case
        Nothing -> error "Could not resolve master; is the repo configured?"
        Just master -> do
          createReference refname master
          resolveReference refname >>= \case
            Nothing -> error "Can not resolve newly created refname"
            Just r -> return r
  parent <- lookupCommit $ Tagged ref
  tree   <- lookupTree $ commitTree parent
  return (parent, tree)

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
  commit refname parent tid message
  return ()

commit refname commit tree message = do
  let sig = defaultSignature
        { signatureName = "James Dabbs"
        , signatureEmail = "jamesdabbs@gmail.com"
        }
  createCommit [commitOid commit] tree sig sig message (Just refname)

write file body = do
  _id <- createBlobUtf8 body
  putEntry file (BlobEntry _id PlainBlob)

old = do
  aid <- createBlobUtf8 "Other file"
  bid <- createBlobUtf8 "Hello, git"
  curry p aid bid

  tid <- createTree $ do
    putEntry "a" (BlobEntry aid PlainBlob)
    putEntry "b" (BlobEntry bid PlainBlob)
  -- p tid

  mhid <- resolveReference "HEAD"
  let pids = case mhid of
        Just r -> [Tagged r]
        Nothing -> []

  let sig = defaultSignature
        { signatureName = "James Dabbs"
        , signatureEmail = "jamesdabbs@gmail.com"
        }
  cid <- createCommit pids tid sig sig "Hello, commit" (Just "HEAD")
  return ()

countCommits ref = do
  mref <- lookupReference ref
  case mref of
    Just (RefObj oid) -> do
      -- id <- parseObjOid "83bb84a"
      cs <- listCommits Nothing $ Tagged oid
      liftIO . putStrLn . tshow $ length cs
    Just (RefSymbolic name) ->
      liftIO . putStrLn $ tshow name
    Nothing -> return ()
