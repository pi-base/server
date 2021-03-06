module Data.Git
  ( Commit
  , LgRepo
  , baseCommit
  , branchExists
  , branchRef
  , commitBranch
  , commitFromLabel
  , commitSha
  , createBranchFromBase
  , fetchRefHead
  , getDir
  , headSha
  , lookupCommittish
  , resetBranch
  , resolveCommittish
  , storeLocked
  , updateBranch
  , writePages
  ) where

import Protolude hiding (withMVar)
import Core
import Model (User(..))

import Control.Lens        (view)
import Data.Loader.Types   (Loader, mkLoader)
import Data.Store.Types    as Store (defaultBranch, storeSettings, storeWriteLock)
import Data.Tagged
import Data.Time.LocalTime (getZonedTime)
import Git
import Git.Libgit2         (LgRepo)

-- TODO: evaluate this
-- It seems like we're either concerned about multiple processes at once, and so
-- need a filesystem lock, or we're threadsafe and don't need to lock at all.
storeLocked :: Git m => m a -> m a
storeLocked action = do
  lock <- view storeWriteLock <$> getStore
  withMVar lock $ const action

commitBranch :: Git m
             => BranchName
             -> TreeOid LgRepo
             -> CommitMeta
             -> [Commit LgRepo]
             -> m Sha
commitBranch branch tree meta parents = do
  let ref = Ref branch
  (author, committer, message) <- commitSignatures meta
  commit <- createCommit
    (map commitOid parents)
    tree author committer message
    (Just $ refHead ref)
  return $ commitSha commit

updateBranch :: Git m
             => BranchName
             -> CommitMeta
             -> (Loader -> TreeT LgRepo m a)
             -> m (a, Sha)
updateBranch branch meta handler = storeLocked $ do
  let ref = Ref branch
  parent   <- fetchRefHead ref
  tree     <- lookupTree $ commitTree parent
  loader   <- mkLoader parent
  (result, newTree) <- withTree tree $ handler loader
  sha <- commitBranch branch newTree meta [parent]
  return (result, sha)

fetchRefHead :: Git m => Ref -> m (Commit LgRepo)
fetchRefHead ref = resolveReference (refHead ref) >>= \case
  Nothing    -> notFound "Ref" $ show ref
  Just found -> lookupCommit $ Tagged found

commitSha :: Commit LgRepo -> Sha
commitSha cmt = case commitOid cmt of
  (Tagged oid) -> show oid

getDir :: Git m => Tree LgRepo -> [TreeFilePath] -> m (Maybe (Tree LgRepo))
getDir = foldM cd . Just
  where
    cd :: Git m => Maybe (Tree LgRepo) -> TreeFilePath -> m (Maybe (Tree LgRepo))
    cd mtree path = case mtree of
      Just tree -> treeEntry tree path >>= \case
        Just (TreeEntry _id) -> Just <$> lookupTree _id
        _ -> return Nothing
      _ -> return Nothing

branchRef :: Branch -> Ref
branchRef = Ref . branchName

branchExists :: Git m => Branch -> m Bool
branchExists branch = do
  existing <- resolveReference $ refHead $ branchRef branch
  return $ isJust existing

createBranchFromBase :: Git m => Branch -> Ref -> m ()
createBranchFromBase new base = lookupReference (refHead base) >>= \case
  Nothing -> notFound "Branch" $ show base
  Just b  -> createReference (refHead $ branchRef new) b

commitFromLabel :: Git m => Maybe Text -> m (Commit LgRepo)
commitFromLabel Nothing = baseCommit
commitFromLabel (Just label) = do
  mref <- resolveCommittish $ CommitRef $ Ref label
  case mref of
    Just ref -> return ref
    Nothing -> do
      msha <- resolveCommittish $ CommitSha label
      maybe baseCommit return msha

baseCommit :: Git m => m (Commit LgRepo)
baseCommit = do
  branch <- view (storeSettings . defaultBranch) <$> getStore
  Just moid <- lookupCommittish . CommitRef . Ref $ branch
  lookupCommit $ Tagged moid

resetBranch :: Git m => Branch -> Committish -> m Sha
resetBranch branch commish = lookupCommittish commish >>= \case
  Nothing -> notFound "committish" $ show commish
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

writePages :: Git m => [(TreeFilePath, Text)] -> TreeT LgRepo m ()
writePages pages = forM_ pages $ \(path, contents) ->
  (lift $ createBlobUtf8 contents) >>= putBlob path

refHead :: Ref -> Text
refHead (Ref name) = "refs/heads/" <> name

headSha :: Git m => BranchName -> m Sha
headSha branchName = do
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
