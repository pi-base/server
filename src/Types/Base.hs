module Types.Base
  ( BranchAccess(..)
  ) where

import Protolude
import GHC.Generics (Generic(..))

data BranchAccess = BranchRead | BranchWrite | BranchAdmin
  deriving (Show, Read, Eq, Ord, Enum, Generic)
