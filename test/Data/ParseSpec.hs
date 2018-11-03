module Data.ParseSpec (spec) where

import Test.Import

import Data.Parse

import Git (commitTree, lookupTree)

import qualified Data.Git as Git

spec :: Runner -> TestM TestTree
spec run = do
  tree <- do
    commit <- Git.baseCommit
    lookupTree $ commitTree commit

  specify "Data.Parse" $ before (run reset) $ do
    it "can parse spaceIds" $ do
      ids <- run $ sourceToList $ spaceIds tree
      length ids `shouldBe` 135

    it "can parse a space object" $ do
      s <- run $ space tree $ Id "S000001"
      spaceName s `shouldBe` "Discrete topology on a two-point set"

    it "can parse traits for a space" $ do
      ids <- run $ sourceToList $ spaceTraitIds (Id "S000001") tree
      ids `shouldBe` map Id ["P000016", "P000024", "P000036", "P000042", "P000052", "P000078"]

    it "can parse theoremIds" $ do
      ids <- run $ sourceToList $ theoremIds tree
      length ids `shouldBe` 186
