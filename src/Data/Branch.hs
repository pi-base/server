module Data.Branch
  ( Branch(..)
  , Name
  , forUser
  , name
  ) where

import Import

import Persist.Backend.DB.Model (Branch(..), User(..))

type Name = Text

name :: Branch -> Name
name = branchName

forUser :: User -> Branch
forUser user = Branch ("users/" <> userEmail user)
