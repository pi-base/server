{-# LANGUAGE TemplateHaskell #-}
module Data.Branch
  ( Branch'(..)
  , Branch
  , Id
  , Name
  , forUser
  , name
  ) where

import Import

import qualified Data.User as User

type Name = Text

data Branch' f = Branch
  { _name :: Columnar f Name
  } deriving Generic

makeLenses ''Branch'

type Branch = Branch' Identity
type Id     = PrimaryKey Branch' Identity

deriving instance Show Branch
deriving instance Eq   Branch

instance Ord Branch where
  compare = compare `on` _name

deriving instance Show Id
deriving instance Eq   Id

instance Beamable Branch'

instance Table Branch' where
  data PrimaryKey Branch' f = Id (C f Text)
    deriving (Generic, Beamable)
  primaryKey = Id . _name

forUser :: User.User -> Branch
forUser user = Branch $ "users/" <> user ^. User.email
