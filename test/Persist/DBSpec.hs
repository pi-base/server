module Persist.DBSpec (spec) where

import Test.Import
import Test.Fixtures

import Test.Hspec (shouldThrow)

import Persist.DB as DB

import qualified Data.Branch as Branch
import qualified Data.Token  as Token

spec :: Env -> IO TestTree
spec env = testSpec "Persist.DB" $ do
  let
    branchName = "test/ensureBranch"
    branch = Branch branchName

  describe "with a live connection" $ do
    let
      eval action = withRollback env $ do
        -- resetDB env -- TODO: run in transaction
        action
          & DB.runIO env
          & check
          & runM

    describe "allBranches" $ do
      it "is empty by default" $ eval $ do
        [] <== allBranches

    describe "ensureBranch" $ do
      it "can create a branch" $ eval $ do
        [] <== allBranches
        ensureBranch branch
        [branch] <== allBranches

      it "does not create duplicate branches" $ eval $ do
        ensureBranch branch
        [branch] <== allBranches
        ensureBranch branch
        [branch] <== allBranches

    describe "findBranch" $ do
      it "can find a branch by name" $ eval $ do
        ensureBranch branch
        Just branch <== findBranch branchName

      it "returns Nothing if the branch does not exist" $ eval $ do
        Nothing <== findBranch branchName

    describe "checkBranchAccess" $ do
      it "returns None by default" $ eval $ do
        createUser james
        ensureBranch branch
        None <== checkBranchAccess james branchName

    describe "grantBranchAccess" $ do
      it "can grant access to users" $ eval $ do
        createUser james
        ensureBranch branch

        grantBranchAccess james branchName Write
        Write <== checkBranchAccess james branchName

    describe "createToken" $ do
      it "can create a new token" $ eval $ do
        token <- createToken james
        Just james <== userForToken (token ^. Token.uuid)

  describe "with a disallowed connection" $ do
    let
      eval action = do
        action
          & DB.disallowed
          & check
          & runM

    it "allows actions which don't use the database" $ eval $ do
      () === ()

    it "does not allow actions which use the database" $ do
      eval allBranches `shouldThrow` (== Disallowed)
