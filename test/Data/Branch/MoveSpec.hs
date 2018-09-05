{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
module Data.Branch.MoveSpec (spec) where

import Protolude
import Test.Import

import qualified Data.Map as M

import qualified Data.Branch   as Branch
import qualified Data.Storable as Store

import Data.Branch.Move

spec :: IO (TestApp App) -> IO TestTree
spec getApp = do
  graph <- mkGraph getApp

  let
    g :: forall a. G a -> IO a
    g = runG graph

  testSpec "Move" $ do
    it "can move by maps" $ g $ do
      user   <- mkUser "move"
      branch <- Branch.base >>= mkBranch "testing/move"

      void $ Branch.update branch user "Move files" $ \_ -> do
        moveMaps
          (M.fromList [(finiteDiscrete,  999)])
          (M.fromList [(compact,         999)])
          (M.fromList [(finiteIsCompact, 999)])

      Just s <- Store.find @Space branch 999
      spaceName s `shouldBe` "Discrete topology on a two-point set"

      Just p <- Store.find @Property branch 999
      propertyName p `shouldBe` "Compact"

      Just t <- Store.find @(Theorem PropertyId) branch 999
      theoremDescription t `shouldBe` "Follows from the definition on page 18 of {{doi:10.1007/978-1-4612-6290-9}}."

      Just t1 <- Store.find @(Trait SpaceId PropertyId) branch (999, 999)
      _traitValue t1 `shouldBe` True

      Just t2 <- Store.find @(Trait SpaceId PropertyId) branch (indiscrete, 999)
      _traitValue t2 `shouldBe` True
