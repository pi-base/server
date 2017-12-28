-- Includes types which are used in the database models, and so need to be defined
-- before any of the model-related types are available
{-# LANGUAGE DeriveGeneric #-}
module Types.Base
  ( BranchAccess(..)
  ) where

import Prelude
import GHC.Generics (Generic(..))

data BranchAccess = BranchRead | BranchWrite | BranchAdmin
  deriving (Show, Read, Eq, Enum, Generic)
