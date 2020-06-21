{-# LANGUAGE TemplateHaskell #-}
module Data.User
  ( User'(..)
  , User
  , Id
  , email
  , name
  , reviewer
  , system
  ) where

import Import

data User' f = User
  { _name     :: C f Text
  , _email    :: C f Text -- TODO: unique constraint
  , _reviewer :: C f Bool
  } deriving Generic

makeLenses ''User'

type User = User' Identity
type Id   = PrimaryKey User' Identity

deriving instance Show User
deriving instance Eq   User

deriving instance FromJSON User
deriving instance ToJSON   User

deriving instance Show Id
deriving instance Eq   Id

instance Ord User where
  compare = compare `on` _email

instance Beamable User'

instance Table User' where
  data PrimaryKey User' f = Id (C f Text)
    deriving (Generic, Beamable)
  primaryKey = Id . _email

system :: User
system = User
  { _name     = "Pi-Base"
  , _email    = "hausdorff@pi-base.org"
  , _reviewer = True
  }
