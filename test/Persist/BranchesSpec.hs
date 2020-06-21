module Persist.BranchesSpec (spec) where

import Test.Import hiding (find)

import Persist.Branches as Branches

import qualified Data.Branch         as Branch
import qualified Persist.Auth        as Auth
import qualified Persist.Backend.Git as Git
import qualified Persist.DB          as DB
import qualified Persist.Github      as Github
import qualified Persist.Repo        as Repo

spec :: DB.Env -> IO TestTree
spec db =
  testSpec "Persist.Branches" $ parallel $ do
    api "pure" $ \action ->
      runM
        -- TODO: use runTest and Interpreter to simplify some of these test setups
        . check
        . Auth.evalState
        . DB.runIO db
        . Github.runState
        . Repo.ignore
        . Branches.runState testMaster $ do
            create (Branch._name testMaster) >> action

    api "io" $ \action -> withTempRepo $ \env -> do
      Git.run env $ Git.createMaster testMaster
      resetDB db
      runM
        . check
        . Auth.evalState
        . DB.runIO db
        . Github.runState
        . Repo.runIO env
        . Branches.runIO testMaster $ do
            scan >> action

    describe "when github is unreachable" $ do
      let
        eval :: Sem '[Branches, Repo.Repo, Auth.Auth, Github.Github, DB.DB, Expect, Embed IO] a -> IO a
        eval action = withTempRepo $ \env -> do
          Git.run env $ Git.createMaster testMaster
          resetDB db
          runM
            . check
            . DB.runIO db
            . Github.unreachable
            . Auth.evalState
            . Repo.runIO env
            . Branches.runIO testMaster
            $ action

      it "raises an error" $ eval $ void $ do
        expectLeft =<< submit =<< create "test/working"

api :: String
    -> (forall a. Sem '[Branches, Repo.Repo, Github.Github, DB.DB, Auth.Auth, Expect, Embed IO] a -> IO a)
    -> Spec
api label eval = describe label $ do
  describe "all" $ do
    it "only includes master by default" $ eval $ do
      [testMaster] <== all

    it "includes any newly created branches" $ eval $ do
      other <- create "test/working"

      [testMaster, other] <== fmap sort all

  describe "current" $ do
    it "is master by default" $ eval $ do
      testMaster <== current

    it "can checkout another branch" $ eval $ do
      other <- create "test/working"
      checkout other

      other <== current

  describe "master" $ do
    it "always returns the master branch" $ eval $ do
      other <- create "test/working"
      checkout other

      testMaster <== master

  describe "find" $ do
    it "finds branches that exists" $ eval $ do
      Just testMaster <== find (testMaster ^. Branch.name)

    it "returns nothing when not found" $ eval $ do
      Nothing <== find "test/working"

  describe "submit" $ do
    it "creates a pull request without error" $ eval $ do
      pr <- expectRight =<< submit =<< create "test/working"

      [pr] <== Github.pullRequests
