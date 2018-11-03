module Data.Branch
  ( access
  , all
  , byName
  , claimUserBranches
  , commit
  , ensureBaseBranch
  , ensureUserBranch
  , fetch
  , find
  , forUser
  , grant
  , headSha
  , push
  , ref
  , reset
  , reset_
  , tree
  , userBranches
  ) where

import Core hiding (all, find, head, on, isNothing, (^.))

import Data.Attoparsec.Text
import Database.Esqueleto
import Database.Persist (selectList)

import qualified Data.Git as Git
import qualified Data.Map.Strict as M
import           Git

import           Data.Store   (fetchBranch, getDefaultBranch, pushBranch)
import           Model        (Unique(..))
import           Util         (findOrCreate, repsertBy)

type Name = Text
type OwnerName = Text

find :: DB m => Text -> m (Maybe (Entity Branch))
find = db . getBy . UniqueBranchName

access :: DB m => Entity User -> Entity Branch -> m (Maybe BranchAccess)
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

all :: DB m => m [Branch]
all = do
  entities <- db $ selectList [] []
  return $ map entityVal entities

userBranches :: (DB m, Git m) => Entity User -> m [BranchStatus]
userBranches (Entity _id User{..}) = if userIsReviewer
  then all >>= mapM (\b -> b `withAccess` BranchAdmin)
  else do
    base <- ensureBaseBranch
    grantedPairs <- db $ select $
      from $ \(branch `InnerJoin` ub) -> do
      on (branch ^. BranchId ==. ub ^. UserBranchBranchId)
      where_ $ ub ^. UserBranchUserId ==. val _id
      return (branch, ub ^. UserBranchRole)
    forM ((base, Value BranchRead) : grantedPairs) $
      \(Entity _ branch, Value role) -> branch `withAccess` role

withAccess :: (DB m, Git m) => Branch -> BranchAccess -> m BranchStatus
withAccess b r = do
  sha <- headSha b
  return $ BranchStatus b sha r

claimUserBranches :: (DB m, Git m) => m ()
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
    buildOwnerMap :: DB m => [OwnerName] -> m (M.Map OwnerName UserId)
    buildOwnerMap names = do
      ownerPairs <- db $ select $
        from $ \users -> do
        where_ $ users ^. UserName `in_` valList names
        return (users ^. UserName, users ^. UserId)
      return . M.fromList $ map (\(Value a, Value b) -> (a, b)) ownerPairs

commit :: Git m => Branch -> m (Commit LgRepo)
commit branch = do
  head <- Git.resolveCommittish . CommitRef $ Ref $ branchName branch
  maybe (notFound "Branch" $ branchName branch) return head

forUser :: Entity User -> Branch
forUser (Entity _id User{..}) = Branch
  { branchName    = "users/" <> userEmail
  , branchOwnerId = Just _id
  }

grant :: DB m => Entity User -> Entity Branch -> BranchAccess -> m ()
grant u b lvl = void $ repsertBy selector ub
  where
    selector UserBranch{..} = UniqueUserBranch userBranchUserId userBranchBranchId
    ub = UserBranch (entityKey u) (entityKey b) lvl

ensureBranch :: (DB m, Git m) => Branch -> m (Entity Branch)
ensureBranch branch = do
  -- In git repo
  found <- Git.branchExists branch
  unless found $ do
    defaultBranch <- getDefaultBranch
    Git.createBranchFromBase branch $ Ref defaultBranch

  -- In database
  repsertBy (UniqueBranchName . branchName) branch

ensureUserBranch :: (DB m, Git m) => Entity User -> m (Entity Branch)
ensureUserBranch user = do
  branch <- ensureBranch $ forUser user
  grant user branch BranchAdmin
  return branch

ensureBaseBranch :: (DB m, Git m) => m (Entity Branch)
ensureBaseBranch = do
  defaultBranch <- getDefaultBranch
  ensureBranch $ Branch defaultBranch Nothing

reset :: Git m => Branch -> Committish -> m Sha
reset = Git.resetBranch

reset_ :: Git m => Branch -> Committish -> m ()
reset_ b = void . Git.resetBranch b

scanRepo :: Git m => m [(Name, Maybe OwnerName)]
scanRepo = do
  refs <- Git.listReferences
  return $ foldr f [] refs
  where
    f :: RefName -> [(Name, Maybe OwnerName)] -> [(Name, Maybe OwnerName)]
    f name acc = either (const acc) (: acc) $ parseOnly refParser name

byName :: (DB m, Git m) => Text -> m Branch
byName name = do
  (Entity _ branch) <- findOrCreate (UniqueBranchName . branchName) $ Branch name Nothing
  return branch

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

headSha :: Git m => Branch -> m Sha
headSha = Git.headSha . branchName

fetch :: (Git m, MonadLogger m) => Branch -> m ()
fetch = fetchBranch

push :: (Git m, MonadLogger m) => Branch -> m ()
push = pushBranch

ref :: Branch -> Ref
ref = Git.branchRef

tree :: Git m => Branch -> m (Tree LgRepo)
tree branch = do
  c <- commit branch
  lookupTree $ commitTree c
