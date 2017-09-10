module Data.Git
  ( getDir
  , modifyGitRef
  , openRepo
  -- , useRepo
  , useRef
  , writeContents
  , ensureUserBranch
  , commitVersion
  , writePages
  , resetRef
  , updateRef
  , updateRef'
  , move
  , lookupCommittish
  , userBranch
  ) where

-- TODO: clean up duplicates, unused exports

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

useRef :: MonadStore m
       => Ref
       -> CommitMeta
       -> TreeT LgRepo m (Either Error a)
       -> m (Either Error (Version, a))
useRef ref meta handler = do
  Store{..} <- getStore
  mpt <- runLgRepository storeRepo $
    resolveReference (refHead ref) >>= \case
      Nothing    -> return Nothing
      Just found -> runLgRepository storeRepo $ do
        parent <- lookupCommit $ Tagged found
        tree   <- lookupTree $ commitTree parent
        return $ Just (parent, tree)
  case mpt of
    Just (parent, tree) -> do
      (result, newTree) <- withTree tree handler
      case result of
        Left  err -> return $ Left err
        Right val -> do
          (author, committer, message) <- commitSignatures meta
          commit <- createCommit [commitOid parent] newTree author committer message (Just $ refHead ref)
          return $ Right (commitVersion commit, val)
    Nothing -> error "Can't find ref" -- FIXME

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
ensureUserBranch user = do
  let branch = userBranch user
  existing <- resolveReference $ refHead branch
  unless (isJust existing) $
    createRefFromMaster $ userBranch user
  return branch

createRefFromMaster :: MonadStore m => Ref -> m ()
createRefFromMaster ref = do
  let masterRef = Ref "master"
  lookupReference (refHead masterRef) >>= \case
    Nothing -> throwM $ UnknownGitRef masterRef
    Just master -> createReference (refHead ref) master

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

resetRef :: MonadStore m => Ref -> Committish -> m ()
resetRef ref commish = lookupCommittish commish >>= \case
  Just r  -> updateReference (refHead ref) (RefObj r)
  Nothing -> error "Could not find committish to reset"

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

writePages :: MonadGit r m => [(TreeFilePath, Text)] -> TreeT r m ()
writePages pages = forM_ pages $ \(path, contents) ->
  (lift $ createBlobUtf8 contents) >>= putBlob path

refHead :: Ref -> Text
refHead (Ref name) = "refs/heads/" <> name

updateRef' :: MonadStore m
           => Ref -> CommitMeta -> TreeT LgRepo m a -> m (a, Version)
updateRef' ref meta updates = do
  resolveReference (refHead ref) >>= \case
    Nothing -> do
      createRefFromMaster ref
      updateRef' ref meta updates
    Just found -> do
      parent <- lookupCommit $ Tagged found
      tree   <- lookupTree $ commitTree parent
      (result, newTree) <- withTree tree updates
      (author, committer, message) <- commitSignatures meta
      commit <- createCommit [commitOid parent] newTree author committer message (Just $ refHead ref)
      return (result, commitVersion commit)

updateRef :: MonadStore m
          => Ref -> CommitMeta -> TreeT LgRepo m a -> m Version
updateRef ref meta updates = do
  (_, version) <- updateRef' ref meta updates
  return version

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

lookupCommittish :: MonadStore m => Committish -> m (Maybe (Oid LgRepo))
lookupCommittish (CommitRef ref) = resolveReference $ refHead ref
lookupCommittish (CommitSha sha) = Just <$> parseOid sha

move :: (MonadThrow m, MonadGit r m) => TreeFilePath -> TreeFilePath -> TreeT r m ()
move old new = getEntry old >>= \case
  Nothing  -> lift . throwM . NotFound $ "mv: " ++ tshow old
  Just ent -> putEntry new ent >> dropEntry old
