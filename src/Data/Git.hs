module Data.Git
  ( Commit
  , LgRepo
  , baseCommit
  , branchExists
  , branchRef
  , commitFromLabel
  , commitSha
  , createBranchFromBase
  , fetchRefHead
  , getDir
  , headSha
  , lookupCommittish
  , resetBranch
  , resolveCommittish
  , updateBranch
  , writePages
  ) where

import Protolude
import Core
import Model (User(..))

import Data.Tagged
import Data.Time.LocalTime (getZonedTime)
import Git
import Git.Libgit2  (LgRepo)
import Types.Loader (Loader, mkLoader)
import Types.Store  (storeBaseBranch)

updateBranch :: MonadStore m
             => BranchName
             -> CommitMeta
             -> (Loader -> TreeT LgRepo m a)
             -> m (a, Sha)
updateBranch branch meta handler = do
  let ref = Ref branch
  parent   <- fetchRefHead ref
  tree     <- lookupTree $ commitTree parent
  loader   <- mkLoader parent
  (result, newTree) <- withTree tree $ handler loader
  (author, committer, message) <- commitSignatures meta
  commit <- createCommit [commitOid parent] newTree author committer message (Just $ refHead ref)
  return (result, commitSha commit)

fetchRefHead :: MonadStore m => Ref -> m (Commit LgRepo)
fetchRefHead ref = resolveReference (refHead ref) >>= \case
  Nothing    -> notFound "Ref" ref
  Just found -> lookupCommit $ Tagged found

commitSha :: Commit LgRepo -> Sha
commitSha cmt = case commitOid cmt of
  (Tagged oid) -> show oid

getDir :: MonadGit r m => Tree r -> [TreeFilePath] -> m (Maybe (Tree r))
getDir = foldM cd . Just
  where
    cd :: MonadGit r m => Maybe (Tree r) -> TreeFilePath -> m (Maybe (Tree r))
    cd mtree path = case mtree of
      Just tree -> treeEntry tree path >>= \case
        Just (TreeEntry _id) -> Just <$> lookupTree _id
        _ -> return Nothing
      _ -> return Nothing

branchRef :: Branch -> Ref
branchRef = Ref . branchName

branchExists :: MonadStore m => Branch -> m Bool
branchExists branch = do
  existing <- resolveReference $ refHead $ branchRef branch
  return $ isJust existing

createBranchFromBase :: MonadStore m => Branch -> Ref -> m ()
createBranchFromBase new base = lookupReference (refHead base) >>= \case
  Nothing -> notFound "Branch" base
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
      { signatureEmail = "hausdorff@pi-base.org"
      , signatureName  = "Pi-Base"
      , signatureWhen  = time
      }
  return (author, committer, commitMessage)

writePages :: MonadGit r m => [(TreeFilePath, Text)] -> TreeT r m ()
writePages pages = forM_ pages $ \(path, contents) ->
  (lift $ createBlobUtf8 contents) >>= putBlob path

refHead :: Ref -> Text
refHead (Ref name) = "refs/heads/" <> name

headSha :: (MonadIO m, MonadGit LgRepo m) => Branch -> m Sha
headSha Branch{..} = do
  found <- lookupCommittish $ CommitRef $ Ref branchName
  case found of
    Just oid -> return $ show oid
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
