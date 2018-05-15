{-# OPTIONS_GHC -fno-warn-orphans #-}
module Data.ParseSpec (spec) where

import TestImport

import Conduit
import Git.Libgit2 (HasLgRepo(..))

import Core
import Data.Parse

import qualified Data.Git   as Git
import           Data.Store (storeRepo)

type M = ReaderT Store IO

-- TODO: should have MonadStore m => HasLgRepo m
instance HasLgRepo M where
  getRepository = storeRepo <$> ask 
instance MonadStore M where
  getStore = ask

runM :: Store -> M a -> IO a 
runM = flip runReaderT

spec :: IO (TestApp App) -> IO TestTree
spec getApp = do
  store <- appStore . fst <$> getApp

  Just commit <- runM store $ Git.resolveCommittish $ CommitRef "master"

  testSpec "Data.ParseSpec" $ do
    it "can parse spaceIds" $ do
      ids <- runM store $ sourceToList $ spaceIds commit
      length ids `shouldBe` 135

    it "can parse a space object" $ do
      Right s <- runM store $ space commit $ Id "S000001"
      spaceName s `shouldBe` "Discrete topology on a two-point set"

    it "can parse traits for a space" $ do
      ids <- runM store $ sourceToList $ spaceTraitIds (Id "S000001") commit
      ids `shouldBe` map Id ["P000016", "P000024", "P000036", "P000042", "P000052", "P000078"]
