module Data.Git
  ( Commit
  , LgRepo
  , branchExists
  , branchRef
  , commitFromLabel
  , commitSha
  , createBranchFromBase
  , getDir
  , headSha
  , openRepo
  , resetBranch
  , resolveCommittish
  , updateRef
  , updateBranch
  , useRef
  , useRepo
  , writePages
  ) where

import Core
import Model (User(..))

import Data.Tagged
import Data.Time.LocalTime (getZonedTime)
import Git
import Git.Libgit2  (LgRepo, openLgRepository, runLgRepository)
import Types.Loader (Loader, mkLoader)
import Types.Store  (storeRepo, storeBaseRef)

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
  repo <- storeRepo <$> getStore
  runLgRepository repo handler

useRef :: MonadStore m
       => Ref
       -> CommitMeta
       -> (Loader -> TreeT LgRepo m (Either Error a))
       -> m (Either Error (Version, a))
useRef ref meta handler = do
  repo <- storeRepo <$> getStore
  mpt <- runLgRepository repo $
    resolveReference (refHead ref) >>= \case
      Nothing    -> return Nothing
      Just found -> runLgRepository repo $ do
        parent <- lookupCommit $ Tagged found
        tree   <- lookupTree $ commitTree parent
        return $ Just (parent, tree)
  case mpt of
    Just (parent, tree) -> do
      loader <- mkLoader parent
      (result, newTree) <- withTree tree $ handler loader
      case result of
        Left  err -> return $ Left err
        Right val -> do
          (author, committer, message) <- commitSignatures meta
          commit <- createCommit [commitOid parent] newTree author committer message (Just $ refHead ref)
          return $ Right (Version $ commitSha commit, val)
    Nothing -> error "Can't find ref" -- FIXME

-- TODO: replace useRef with this entirely
updateBranch :: MonadStore m
             => Branch
             -> CommitMeta
             -> (Loader -> TreeT LgRepo m a)
             -> m (a, Sha)
updateBranch branch meta handler = useRef ref meta handler' >>= \case
  Left err -> throw err
  Right (Version sha, a) -> return (a, sha)
  where
    ref = branchRef branch
    handler' loader = Right <$> handler loader


commitSha :: Commit LgRepo -> Sha
commitSha cmt = case commitOid cmt of
  (Tagged oid) -> tshow oid

commitVersion :: Commit LgRepo -> Version
commitVersion = Version . commitSha

cd :: (MonadGit r m) => Tree r -> TreeFilePath -> m (Either Error (Tree r))
cd tree path = treeEntry tree path >>= \case
  Just (TreeEntry _id) -> lookupTree _id >>= return . Right
  _ -> return . Left $ NotATree path

getDir :: MonadGit r m => Tree r -> [TreeFilePath] -> m (Either Error (Tree r))
getDir tree = foldM cd' $ Right tree
  where
    cd' :: MonadGit r m => Either Error (Tree r) -> TreeFilePath -> m (Either Error (Tree r))
    cd' etree path = either (return . Left) (flip cd path) etree

branchRef :: Branch -> Ref
branchRef = Ref . branchName

branchExists :: MonadStore m => Branch -> m Bool
branchExists branch = do
  existing <- resolveReference $ refHead $ branchRef branch
  return $ isJust existing

createBranchFromBase :: MonadStore m => Branch -> Branch -> m ()
createBranchFromBase new base = do
  lookupReference (refHead $ branchRef base) >>= \case
    Nothing -> throwM $ UnknownGitRef $ branchRef base
    Just b  -> createReference (refHead $ branchRef new) b

createRefFromBase :: MonadStore m => Ref -> m ()
createRefFromBase ref = do
  base <- storeBaseRef <$> getStore
  lookupReference (refHead base) >>= \case
    Nothing -> throwM $ UnknownGitRef base
    Just b -> createReference (refHead ref) b

commitFromLabel :: MonadStore m => Maybe Text -> m (Commit LgRepo)
commitFromLabel Nothing = baseCommit
commitFromLabel (Just label) = do
  mref <- resolveCommittish $ CommitRef $ Ref label
  case mref of
    Just ref -> return ref
    Nothing -> do
      msha <- resolveCommittish $ CommitSha label
      maybe baseCommit return msha


baseCommit :: MonadStore m => m (Commit LgRepo)
baseCommit = do
  base <- CommitRef . storeBaseRef <$> getStore
  (Just moid) <- lookupCommittish base
  lookupCommit $ Tagged moid

resetBranch :: MonadStore m => Branch -> Committish -> m Sha
resetBranch branch commish = lookupCommittish commish >>= \case
  Nothing -> error "Could not find committish to reset"
  Just r -> do
    updateReference (refHead $ branchRef branch) (RefObj r)
    return $ renderOid r

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
      createRefFromBase ref
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

headSha :: MonadGit LgRepo m => Branch -> m Sha
headSha Branch{..} = do
  found <- lookupCommittish $ CommitRef $ Ref branchName
  case found of
    Just oid -> return $ tshow oid
    Nothing -> error $ "Could not find branch " <> show branchName

lookupCommittish :: MonadGit LgRepo m => Committish -> m (Maybe (Oid LgRepo))
lookupCommittish (CommitRef ref) = resolveReference $ refHead ref
lookupCommittish (CommitSha sha) = Just <$> parseOid sha

resolveCommittish :: MonadGit LgRepo m => Committish -> m (Maybe (Commit LgRepo))
resolveCommittish c = do
  moid <- lookupCommittish c
  case moid of
    Nothing  -> return $ Nothing
    Just oid -> do
      commit <- lookupCommit $ Tagged oid
      return $ Just commit
