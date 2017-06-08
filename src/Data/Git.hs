module Data.Git
  ( getDir
  , modifyGitRef
  , openRepo
  , useRepo
  , writeContents
  , ensureUserBranch
  , commitVersion
  , lookupCommitish
  ) where

import Core
import Model (User(..))

import Data.Tagged
import Data.Time.LocalTime (getZonedTime)
import Git
import Git.Libgit2 (LgRepo, openLgRepository, runLgRepository)

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

ensureUserBranch :: MonadStore m => User -> m Text
ensureUserBranch User{..} = useRepo $ do
  let branch = "users/" <> userName
      branchRef = "refs/heads/" <> branch
  existing <- resolveReference branchRef
  unless (isJust existing) $ do
    lookupReference "refs/heads/master" >>= \case
      Nothing     -> throwM NoMaster
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
              -> m (Either String Version)
writeContents user message files = do
  branch <- ensureUserBranch user

  ec <- modifyGitRef user branch message $ do
    forM_ files $ \(path, contents) -> do
      (lift $ createBlobUtf8 contents) >>= putBlob path
  return $ commitVersion <$> ec

lookupCommitish :: MonadGit r m => Committish -> m (Maybe (Oid r))
lookupCommitish (Ref ref) = resolveReference $ "refs/heads/" <> ref
lookupCommitish (Sha sha) = Just <$> parseOid sha
