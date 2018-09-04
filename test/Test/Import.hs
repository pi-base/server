{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
module Test.Import
  ( module Test.Import
  , module X
  ) where

import Prelude
import Protolude ((<>), void)

import Database.Persist as X (Entity(..))
import Test.Fixtures    as X
import Test.Tasty       as X
import Test.Tasty.Hspec as X hiding (shouldBe)
import Yesod.Test       as X (TestApp)

import Foundation   as X
import Model        as X

import Core         as X
import Graph.Common as X

import           Data.Id         as Id
import qualified Data.Git        as Git
import           Handler.Helpers (ensureUser)

-- TODO:
-- * extract these helpers to factories
-- * add lenses for overriding defaults

mkUser :: Text -> G User
mkUser name = do
  Entity _ user <- ensureUser $ User name name (name <> "@example.com") name False
  return user

mkBranch :: Text -> Branch -> G Branch
mkBranch name base = do
  let branch = Branch name Nothing
  found <- Git.branchExists branch
  if found
    then void $ Git.resetBranch branch $ CommitRef $ Git.branchRef base
    else void $ Git.createBranchFromBase branch $ Git.branchRef base
  return branch
