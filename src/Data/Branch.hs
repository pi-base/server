module Data.Branch
  ( access
  , all
  , base
  , claimUserBranches
  , commit
  , ensureBaseBranch
  , ensureUserBranch
  , fetch
  , find
  , forUser
  , grant
  , headSha
  , loader
  , push
  , ref
  , reset
  , tree
  , update
  , userBranches
  ) where

import Protolude hiding (all, find, from, head, on, isNothing)

import Data.Attoparsec.Text
import Database.Esqueleto   hiding (update)
import Database.Persist     (selectList)

import           Core
import qualified Data.Git as Git
import qualified Data.Map.Strict as M
import           Git

import           Data.Helpers (findOrCreate, repsertBy)
import           Data.Store   (fetchBranch, pushBranch, storeAutoSync, storeBaseBranch)
import           Model        (Unique(..))
import           Types.Loader (Loader, mkLoader)

type Name = Text
type OwnerName = Text

find :: MonadDB m => Text -> m (Maybe (Entity Branch))
find = db . getBy . UniqueBranchName

access :: MonadDB m => Entity User -> Entity Branch -> m (Maybe BranchAccess)
access (Entity userId User{..}) (Entity branchId Branch{..}) = if userIsReviewer
  then return $ Just BranchAdmin
  else case branchOwnerId of
    -- TODO: we can probably remove the ownerId concept entirely
    Just ownerId -> if ownerId == userId
      then return $ Just BranchAdmin
      else checkGrants Nothing
    Nothing -> checkGrants $ Just BranchRead
  where
    checkGrants fallback = do
      mub <- db $ getBy $ UniqueUserBranch userId branchId
      case mub of
        Just (Entity _ ub) -> return $ Just $ userBranchRole ub
        _ -> return fallback

all :: MonadDB m => m [Branch]
all = do
  entities <- db $ selectList [] []
  return $ map entityVal entities

userBranches :: (MonadDB m, MonadStore m) => Entity User -> m [BranchStatus]
userBranches (Entity _id User{..}) = if userIsReviewer
  then all >>= mapM (\b -> b `withAccess` BranchAdmin)
  else do
    b <- ensureBaseBranch
    grantedPairs <- db $ select $
      from $ \(branch `InnerJoin` ub) -> do
      on (branch ^. BranchId ==. ub ^. UserBranchBranchId)
      where_ $ ub ^. UserBranchUserId ==. val _id
      return (branch, ub ^. UserBranchRole)
    forM ((b, Value BranchRead) : grantedPairs) $
      \(Entity _ branch, Value role) -> branch `withAccess` role

withAccess :: (MonadDB m, MonadStore m) => Branch -> BranchAccess -> m BranchStatus
withAccess b r = do
  sha <- Git.headSha b
  return $ BranchStatus b sha r

claimUserBranches :: (MonadDB m, MonadStore m) => m ()
claimUserBranches = do
  names    <- scanRepo
  ownerMap <- buildOwnerMap . catMaybes $ map snd names

  forM_ names $ \(name, mownerName) -> do
    let mownerId = mownerName >>= \ownerName -> M.lookup ownerName ownerMap
    case mownerId of
      Nothing ->
        void $ findOrCreate (UniqueBranchName . branchName) $ Branch name Nothing
      Just ownerId -> void $ do
        (Entity branchId _) <- findOrCreate (UniqueBranchName . branchName) $ Branch name (Just ownerId)
        findOrCreate (\UserBranch{..} -> UniqueUserBranch userBranchUserId userBranchBranchId) $ UserBranch ownerId branchId BranchAdmin
  where
    buildOwnerMap :: MonadDB m => [OwnerName] -> m (M.Map OwnerName UserId)
    buildOwnerMap names = do
      ownerPairs <- db $ select $
        from $ \users -> do
        where_ $ users ^. UserName `in_` valList names
        return (users ^. UserName, users ^. UserId)
      return . M.fromList $ map (\(Value a, Value b) -> (a, b)) ownerPairs

commit :: MonadStore m => Branch -> m (Commit LgRepo)
commit branch = do
  head <- Git.resolveCommittish . CommitRef $ Ref $ branchName branch
  case head of
    Just c  -> return c
    Nothing -> notFound "Branch" $ branchName branch

-- TODO: check for an in-memory loader for this branch
loader :: MonadStore m => Branch -> m Loader
loader b = commit b >>= mkLoader

forUser :: Entity User -> Branch
forUser (Entity _id User{..}) = Branch
  { branchName    = "users/" <> userEmail
  , branchOwnerId = Just _id
  }

grant :: MonadDB m => Entity User -> Entity Branch -> BranchAccess -> m ()
grant u b lvl = void $ repsertBy selector ub
  where
    selector UserBranch{..} = UniqueUserBranch userBranchUserId userBranchBranchId
    ub = UserBranch (entityKey u) (entityKey b) lvl

ensureBranch :: (MonadDB m, MonadStore m) => Branch -> m (Entity Branch)
ensureBranch branch = do
  -- In git repo
  found <- Git.branchExists branch
  unless found $ do
    b <- Ref . storeBaseBranch <$> getStore
    Git.createBranchFromBase branch b

  -- In database
  repsertBy (UniqueBranchName . branchName) branch

ensureUserBranch :: (MonadDB m, MonadStore m) => Entity User -> m (Entity Branch)
ensureUserBranch user = do
  branch <- ensureBranch $ forUser user
  grant user branch BranchAdmin
  return branch

base :: (MonadDB m, MonadStore m) => m Branch
base = entityVal <$> ensureBaseBranch

ensureBaseBranch :: (MonadDB m, MonadStore m) => m (Entity Branch)
ensureBaseBranch = do
  name <- storeBaseBranch <$> getStore
  ensureBranch $ Branch name Nothing

reset :: MonadStore m => Branch -> Committish -> m Sha
reset = Git.resetBranch

update :: (MonadStore m, MonadLogger m)
       => Branch
       -> User
       -> Text
       -> (Loader -> TreeT LgRepo m a)
       -> m (a, Sha)
update branch user message handler = do
  let meta = CommitMeta user message
  result <- Git.updateBranch (branchName branch) meta handler
  sync   <- storeAutoSync <$> getStore
  when sync $ background $ pushBranch branch
  return result

scanRepo :: MonadStore m => m [(Name, Maybe OwnerName)]
scanRepo = do
  refs <- Git.listReferences
  return $ foldr f [] refs
  where
    f :: RefName -> [(Name, Maybe OwnerName)] -> [(Name, Maybe OwnerName)]
    f name acc = either (const acc) (: acc) $ parseOnly refParser name

refParser :: Parser (Name, Maybe OwnerName)
refParser = do
  _ <- "refs/heads/"
  user <|> system
  where
    user = do
      _ <- "users/"
      userName <- takeText
      return ("users/" <> userName, Just userName)
    system = do
      name <- takeText
      return (name, Nothing)

-- TODO: lower-level functions should take a BranchName; only these should take a Branch
headSha :: MonadStore m => Branch -> m Sha
headSha = Git.headSha

fetch :: (MonadStore m, MonadLogger m) => Branch -> m ()
fetch = fetchBranch

push :: (MonadStore m, MonadLogger m) => Branch -> m ()
push = pushBranch

ref :: Branch -> Ref
ref = Git.branchRef

tree :: MonadStore m => Branch -> m (Tree LgRepo)
tree branch = do
  c <- commit branch
  lookupTree $ commitTree c

-- FIXME: this should enqueue work in a persistent queue, retry, ...
background :: MonadStore m => m () -> m ()
background action = void action