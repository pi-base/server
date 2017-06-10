module Data.Git
  ( getDir
  , modifyGitRef
  , openRepo
  , useRepo
  , writeContents
  , ensureUserBranch
  , commitVersion
  , lookupCommitish
  , writePages
  , updateRef
  ) where

-- TODO: clean up duplicates, unused exports

import Core
import Model (User(..))

import Data.Tagged
import Data.Time.LocalTime (getZonedTime)
import Git
import Git.Libgit2 (LgRepo, openLgRepository, runLgRepository)
import qualified Page (write)

openRepo :: FilePath -> IO LgRepo
openRepo path = openLgRepository $ RepositoryOptions
  { repoPath       = path
  , repoWorkingDir = Nothing
  , repoIsBare     = False
  , repoAutoCreate = False
  }

useRepo :: MonadStore m
        => ReaderT LgRepo m a
        -> m a
useRepo handler = do
  Store{..} <- getStore
  runLgRepository storeRepo handler

commitVersion :: Commit LgRepo -> Version
commitVersion cmt = case commitOid cmt of
  (Tagged oid) -> Version $ tshow oid

getDir :: (MonadGit r m) => Tree r -> TreeFilePath -> m (Either Error (Tree r))
getDir tree path = treeEntry tree path >>= \case
  Just (TreeEntry _id) -> lookupTree _id >>= return . Right
  _ -> return . Left $ NotATree path

userBranch :: User -> Ref
userBranch u = Ref $ "users/" <> userName u

ensureUserBranch :: MonadStore m => User -> m Ref
ensureUserBranch user = useRepo $ do
  let branch = userBranch user
  existing <- resolveReference $ refHead branch
  unless (isJust existing) $ do
    let masterRef = Ref "master"
    lookupReference (refHead masterRef) >>= \case
      Nothing     -> throwM $ UnknownGitRef masterRef
      Just master -> createReference (refHead branch) master
  return branch

modifyGitRef :: MonadStore m
             => User
             -> Ref
             -> CommitMessage
             -> TreeT LgRepo (ReaderT LgRepo m) a
             -> m (Either [Error] (Commit LgRepo))
modifyGitRef user ref message updates = useRepo $ do
  resolveReference (refHead ref) >>= \case
    Nothing -> return $ Left [ UnknownGitRef ref ]
    Just found -> do
      parent <- lookupCommit $ Tagged found
      tree <- lookupTree $ commitTree parent
      (_, newTree) <- withTree tree updates
      (author, committer) <- getSignatures user
      Right <$> createCommit [commitOid parent] newTree author committer message (Just $ refHead ref)

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

commitSignatures :: MonadIO m => CommitMeta -> m (Signature, Signature, Text)
commitSignatures CommitMeta{..} = do
  time <- liftIO getZonedTime
  let
    User{..} = commitUser
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
  return (author, committer, commitMessage)

writePages :: (ToJSON f, MonadGit r m) => [Page f] -> TreeT r m ()
writePages pages = forM_ pages $ \p ->
  let (path, contents) = Page.write p
  in  (lift $ createBlobUtf8 contents) >>= putBlob path

refHead :: Ref -> Text
refHead (Ref name) = "refs/heads/" <> name

updateRef :: (MonadIO m, MonadGit LgRepo m)
          => Ref -> CommitMeta -> TreeT LgRepo m a -> m Version
updateRef ref meta updates = do
  resolveReference (refHead ref) >>= \case
    Nothing -> throwM $ UnknownGitRef ref
    Just found -> do
      parent <- lookupCommit $ Tagged found
      tree   <- lookupTree $ commitTree parent
      (_, newTree) <- withTree tree updates
      (author, committer, message) <- commitSignatures meta
      commitVersion <$> createCommit [commitOid parent] newTree author committer message (Just $ refHead ref)

writeContents :: MonadStore m
              => User
              -> CommitMessage
              -> [(TreeFilePath, Text)]
              -> m (Either [Error] Version)
writeContents user message files = do
  branch <- ensureUserBranch user

  ec <- modifyGitRef user branch message $ do
    forM_ files $ \(path, contents) -> do
      (lift $ createBlobUtf8 contents) >>= putBlob path
  return $ commitVersion <$> ec

lookupCommitish :: MonadGit r m => Committish -> m (Maybe (Oid r))
lookupCommitish (CommitRef ref) = resolveReference $ refHead ref
lookupCommitish (CommitSha sha) = Just <$> parseOid sha
