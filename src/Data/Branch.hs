module Data.Branch
  ( access
  , Data.Branch.all
  , claim
  , commit
  , ensureUserBranch
  , Data.Branch.find
  , headSha
  , reset
  , userBranches
  ) where

import Data.Attoparsec.Text
import Database.Esqueleto
import Database.Persist (selectList)

import           Core
import qualified Data.Git as Git
import qualified Data.Map.Strict as M
import           Git

import           Data.Helpers (findOrCreate)
import           Model        (Unique(..))

type Name = Text
type OwnerName = Text

find :: MonadDB m => Text -> m (Maybe Branch)
find name = do
  meb <- db . getBy $ UniqueBranchName name
  return $ entityVal <$> meb

access :: MonadDB m => Entity User -> Branch -> m (Maybe BranchAccess)
access (Entity _id _) Branch{..} = return $ case branchOwnerId of
  Nothing -> Just BranchRead
  Just ownerId -> if ownerId == _id
    then Just BranchAdmin
    else Nothing

all :: MonadDB m => m [Branch]
all = do
  entities <- db $ selectList [] []
  return $ map entityVal entities

userBranches :: (MonadDB m, MonadStore m) => Entity User -> m [BranchStatus]
userBranches (Entity _id _) = all >>= foldM f []
  where
    f :: MonadStore m => [BranchStatus] -> Branch -> m [BranchStatus]
    f acc branch = case branchOwnerId branch of
      Nothing -> (: acc) <$> build branch BranchRead
      Just ownerId -> if ownerId == _id
        then (: acc) <$> build branch BranchAdmin
        else return acc

    build :: MonadStore m => Branch -> BranchAccess -> m BranchStatus
    build branch role = do
      sha <- Git.useRepo $ Git.headSha branch
      return $ BranchStatus branch sha role

claim :: (MonadDB m, MonadStore m) => m [Entity Branch]
claim = do
  names    <- scanRepo
  ownerMap <- buildOwnerMap . catMaybes $ map snd names

  forM names $ \(name, mownerName) -> do
    let ownerId = mownerName >>= \ownerName -> M.lookup ownerName ownerMap
        branch = Branch name ownerId
    findOrCreate (UniqueBranchName . branchName) branch
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
  head <- Git.useRepo . Git.resolveCommittish . CommitRef $ Ref $ branchName branch
  case head of
    Just c -> return c
    Nothing -> throw $ UnknownGitRef $ Ref $ branchName branch

userBranch :: Entity User -> Branch
userBranch (Entity _id User{..}) = Branch ("users/" <> userName) (Just _id)

ensureUserBranch :: (MonadDB m, MonadStore m) => Entity User -> m Branch
ensureUserBranch user = do
  let branch = userBranch user
  let base   = Branch "development" Nothing -- FIXME
  found <- Git.branchExists branch
  unless found $ Git.createBranchFromBase branch base
  return branch

reset :: MonadStore m => Branch -> Committish -> m Sha
reset = Git.resetBranch

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

headSha :: MonadStore m => Branch -> m Sha
headSha = Git.headSha