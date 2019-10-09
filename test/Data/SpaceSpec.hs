module Data.SpaceSpec (spec) where

import Test.Import
import Test.Fixtures (finiteDiscrete)

import Data.Space
import Data.Structure (validate)

import qualified Data.Id as Id

spec :: IO TestTree
spec = testSpec "Data.Space" $ parallel $ do
  describe "lenses" $ do
    it "can extract parts" $ do
      id finiteDiscrete `shouldBe` Id.fromInt 1
      name finiteDiscrete `shouldBe` "Finite Discrete Topology"

  describe "HKD" $ do
    let
      mspace :: Data.Space.Space' Maybe
      mspace = Space
        { id          = Nothing
        , name        = Just "Name"
        , description = Just "Description"
        , topology    = Just (Just "Topology")
        , aliases     = Just []
        , refs        = Just []
        }

      aspace :: Data.Space.Space' Maybe
      aspace = Space
        { id          = Just (Id.fromInt 1)
        , name        = Just "Name"
        , description = Just "Description"
        , topology    = Just (Just "Topology")
        , aliases     = Just []
        , refs        = Just []
        }

    it "can extract higher-kinded parts" $ do
      id   mspace `shouldBe` Nothing
      name mspace `shouldBe` Just "Name"

    it "can sequence maybes to Nothing" $ do
      validate mspace `shouldBe` Nothing

    it "can sequence maybes to a record" $ do
      let space = Space
            { id          = (Id.fromInt 1)
            , name        = "Name"
            , description = "Description"
            , topology    = (Just "Topology")
            , aliases     = []
            , refs        = []
            }

      validate aspace `shouldBe` Just space
