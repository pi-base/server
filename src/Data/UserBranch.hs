{-# LANGUAGE TemplateHaskell #-}
module Data.UserBranch
  ( UserBranch'(..)
  , UserBranch
  , access
  , branchId
  , userId
  ) where

import Import

import           Data.Access (Access)
import qualified Data.Branch as Branch
import qualified Data.User   as User

data UserBranch' f = UserBranch
  { _userId   :: C f User.Id
  , _branchId :: C f Branch.Id
  , _access   :: C f Access
  } deriving Generic

makeLenses ''UserBranch'

type UserBranch = UserBranch' Identity
type Id         = PrimaryKey UserBranch' Identity

deriving instance Show UserBranch
deriving instance Eq   UserBranch

instance Beamable UserBranch'

instance Table UserBranch' where
  data PrimaryKey UserBranch' f = Id (C f User.Id)
    deriving (Generic, Beamable)
  primaryKey = Id <$> _userId -- <*> _branchId
