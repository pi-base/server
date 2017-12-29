module Data.Branch
  ( access
  , Data.Branch.all
  , claimBranches
  , ensureUserBranch
  , Data.Branch.find
  , reset
  , userBranches
  ) where

import Data.Attoparsec.Text
import Database.Esqueleto

import           Core
import qualified Data.Git as Git
import           Git

find :: MonadStore m => Text -> m (Maybe Branch)
find = error "find"

access :: MonadDB m => Entity User -> Branch -> m BranchAccess
access = error "access"

userBranches :: (MonadDB m, MonadStore m) => Entity User -> m [BranchStatus]
userBranches (Entity _id _) = do
  -- pairs <- db $ select $
  --   from $ \(ub, b) -> do
  --   where_ (ub ^. UserBranchBranchId ==. b ^. BranchId)
  --   where_ (ub ^. UserBranchUserId ==. val _id)
  --   return (b, ub ^. UserBranchRole)

  system <- mapM (\branch -> build branch BranchRead) =<< systemBranches
  -- users  <- traverse (\(Entity _ branch, Value role) -> build branch role) pairs
  return $ system -- <> users
  where
    -- build :: Branch -> BranchAccess -> m BranchStatus
    build branch role = do
      sha <- Git.useRepo $ Git.headSha branch
      return $ BranchStatus branch sha role

systemBranches :: (MonadDB m, MonadStore m) => m [Branch]
systemBranches = return $ map (\n -> Branch n Nothing) [ "master" ]

claimBranches :: (MonadDB m, MonadStore m) => m [Branch]
claimBranches = do
  branches <- Data.Branch.all
  -- forM_ branches $ \name ->
  traceM $ show branches
  return branches

userBranch :: Entity User -> Branch
userBranch (Entity _id User{..}) = Branch ("users/" <> userIdent) (Just _id)

ensureUserBranch :: (MonadDB m, MonadStore m) => Entity User -> m Branch
ensureUserBranch user = do
  let branch = userBranch user
  let base   = Branch "development" Nothing -- FIXME
  found <- Git.branchExists branch
  unless found $ Git.createBranchFromBase branch base
  return branch

reset :: MonadStore m => Branch -> Committish -> m Sha
reset = Git.resetBranch

all :: MonadStore m => m [Branch]
all = do
  refs <- Git.listReferences
  return []
  -- return $ foldr f [] refs
  -- where
  --   f :: RefName -> [Branch] -> [Branch]
  --   f name acc = case parseOnly branchParser name of
  --     Right b -> b : acc
  --     _       -> acc

branchParser :: Parser (Text, Maybe Text)
branchParser = do
  _ <- "refs/heads/"
  userBranch <|> systemBranch
  where
    userBranch = do
      _ <- "users/"
      userName <- takeText
      return ("users/" <> userName, Just userName)
    systemBranch = do
      name <- takeText
      return (name, Nothing)
