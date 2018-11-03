module Data.SpaceSpec (spec) where

import Test.Import

import Data.Space as Space

import qualified Data.Text as T

import qualified Data.Branch as Branch
import qualified Data.Id     as Id

spec :: Runner -> TestM TestTree
spec run = do
  Entity _ base <- Branch.ensureBaseBranch

  specify "Data.Space" $ do
    describe "fetch" $ do
      it "can find a space" $ run $ do
        Space{..} <- Space.fetch base $ Id.fromInt 1
        spaceName `shouldBe` "Discrete topology on a two-point set"

      it "can fail to find a space" $ run $ do
        Left NotFoundError{..} <- try $ Space.fetch base $ Id.fromInt 999999
        nfResource `shouldBe` "Space"
        nfIdentifier `shouldBe` "S999999"

    describe "find" $ do
      it "can find a space" $ run $ do
        Just Space{..} <- Space.find base $ Id.fromInt 1
        spaceName `shouldBe` "Discrete topology on a two-point set"

      it "can fail to find a space" $ run $ do
        mspace <- Space.find base $ Id.fromInt 999999
        mspace `shouldBe` Nothing

    describe "put" $ do
      it "can add a new space" $ run $ do
        let
          user = User "Test" "test@example.com" False
          space = Space
                     { spaceId          = Id.pending
                     , spaceName        = "New Space"
                     , spaceAliases     = []
                     , spaceDescription = ""
                     , spaceTopology    = Nothing
                     , spaceRefs        = []
                     }
          meta = CommitMeta
                   { commitUser    = user
                   , commitMessage = "Create New Space"
                   }
        (Space{..}, _sha) <- Space.put base meta space

        spaceName `shouldBe` "New Space"
        T.unpack (unId spaceId) `shouldStartWith` "s"
