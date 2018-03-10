module Data.Git
  ( Commit
  , LgRepo
  , baseCommit
  , branchExists
  , branchRef
  , commitFromLabel
  , commitSha
  , createBranchFromBase
  , getDir
  , headSha
  , lookupCommittish
  , resetBranch
  , resolveCommittish
  , updateBranch
  , writePages
  ) where

import Core
import Model (User(..))

import Data.Tagged
import Data.Time.LocalTime (getZonedTime)
import Git
import Git.Libgit2  (LgRepo)
import Types.Loader (Loader, mkLoader)
import Types.Store  (storeBaseBranch)

updateBranch :: MonadStore m
             => Branch
             -> CommitMeta
             -> (Loader -> TreeT LgRepo m a)
             -> m (a, Sha)
updateBranch branch meta handler = do
  let ref = branchRef branch
  resolved <- fetchRef ref
  parent   <- lookupCommit $ Tagged resolved
  tree     <- lookupTree $ commitTree parent
  loader   <- mkLoader parent
  (result, newTree) <- withTree tree $ handler loader
  (author, committer, message) <- commitSignatures meta
  commit <- createCommit [commitOid parent] newTree author committer message (Just $ refHead ref)
  return (result, commitSha commit)

fetchRef :: MonadStore m => Ref -> m (Oid LgRepo)
fetchRef ref = resolveReference (refHead ref) >>= \case
  Nothing -> throwM $ UnknownGitRef ref
  Just found -> return found

commitSha :: Commit LgRepo -> Sha
commitSha cmt = case commitOid cmt of
  (Tagged oid) -> tshow oid

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

createBranchFromBase :: MonadStore m => Branch -> Ref -> m ()
createBranchFromBase new base = lookupReference (refHead base) >>= \case
  Nothing -> throwM $ UnknownGitRef base
  Just b  -> createReference (refHead $ branchRef new) b

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
  base <- CommitRef . Ref . storeBaseBranch <$> getStore
  (Just moid) <- lookupCommittish base
  lookupCommit $ Tagged moid

resetBranch :: MonadStore m => Branch -> Committish -> m Sha
resetBranch branch commish = lookupCommittish commish >>= \case
  Nothing -> notFound "committish" commish
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

headSha :: MonadGit LgRepo m => Branch -> m Sha
headSha Branch{..} = do
  found <- lookupCommittish $ CommitRef $ Ref branchName
  case found of
    Just oid -> return $ tshow oid
    Nothing -> notFound "branch" branchName

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

notFound :: (MonadThrow m, Show a) => Text -> a -> m b
notFound resource ident = throw $ NotFound $ NotFoundError resource $ tshow ident
