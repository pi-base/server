module Data.Branch
  ( allBranches
  , ensureUserBranch
  , resetBranch
  , userBranches
  ) where

import Data.Attoparsec.Text
import Database.Esqueleto

import           Core
import qualified Data.Git as Git
import           Git

userBranches :: (MonadDB m, MonadStore m) => Entity User -> m [BranchStatus]
userBranches (Entity _id _) = do
  pairs <- db $ select $
    from $ \(ub, b) -> do
    where_ (ub ^. UserBranchBranchId ==. b ^. BranchId)
    where_ (ub ^. UserBranchUserId ==. val _id)
    return (b, ub ^. UserBranchRole)

  system <- mapM (\branch -> build branch BranchRead) =<< systemBranches
  users  <- traverse (\(Entity _ branch, Value access) -> build branch access) pairs
  return $ system <> users
  where
    -- build :: Branch -> BranchAccess -> m BranchStatus
    build branch access = do
      sha <- Git.useRepo $ Git.headSha branch
      return $ BranchStatus branch sha access

systemBranches :: (MonadDB m, MonadStore m) => m [Branch]
systemBranches = return $ map (\n -> Branch n Nothing) [ "master" ]

claimBranches :: (MonadDB m, MonadStore m) => m [Branch]
claimBranches = return []

userBranch :: Entity User -> Branch
userBranch (Entity _id User{..}) = Branch ("users/" <> userIdent) (Just _id)

ensureUserBranch :: (MonadDB m, MonadStore m) => Entity User -> m Branch
ensureUserBranch user = do
  let branch = userBranch user
  let base   = Branch "development" Nothing -- FIXME
  found <- Git.branchExists branch
  unless found $ Git.createBranchFromBase branch base
  return branch

resetBranch :: MonadStore m => Branch -> Committish -> m Sha
resetBranch = Git.resetBranch

allBranches :: MonadStore m => m [Branch]
allBranches = do
  refs <- Git.listReferences
  return $ foldr f [] refs
  where
    f :: RefName -> [Branch] -> [Branch]
    f name acc = case parseOnly branchParser name of
      Right b -> b : acc
      _       -> acc

branchParser :: Parser Branch
branchParser = do
  _ <- "refs/heads/"
  branchName <- takeText
  let branchOwnerId = Nothing -- FIXME
  return $ Branch {..}
