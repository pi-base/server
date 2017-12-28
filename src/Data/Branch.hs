module Data.Branch
  ( userBranches
  ) where

import Database.Esqueleto

import           Core
import qualified Data.Git as Git

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
