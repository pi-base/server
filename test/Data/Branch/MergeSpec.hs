module Data.Branch.MergeSpec (spec) where

import Protolude
import Test.Import

import Data.Branch.Merge (Merge(..), merge)
import Formula           ((.=))

import qualified Data.Branch   as Branch
import qualified Data.Id       as Id
import qualified Data.Storable as S
import qualified Data.Trait    as Trait

spec :: IO (TestApp App) -> IO TestTree
spec getApp = do
  graph <- mkGraph getApp

  let
    g :: forall a. G a -> IO a
    g = runG graph

  testSpec "Merge" $ do
    it "can merge" $ g $ do
      user  <- mkUser "user"
      admin <- mkUser "admin"

      base <- Branch.base >>= mkBranch "test/merge/into"
      pr   <- mkBranch "test/merge/pr" base

      {-
        Compact => P
        S |= P + ~Compact
      -}
      s <- Id.assign $ Space Id.pending Nothing "S" [] "Desc" Nothing []
      p <- Id.assign $ Property Id.pending Nothing "P" [] "Desc" []

      let
        t1 = Trait (spaceId s) compact False [] ""
        t2 = Trait (spaceId s) (propertyId p) True [] ""
        impl = Implication (compact .= True) (propertyId p .= True)

      t <- Id.assign $ Theorem Id.pending impl Nothing "T" []

      (_, sha) <- Branch.update pr user "Add results" $ \_ -> do
        S.write s
        S.write p
        S.write t
        S.write t1
        S.write t2

      sha' <- merge (Merge { from = pr, into = base }) (CommitMeta admin "Approve merge")
      sha == sha' `shouldBe` False

      Just s' <- find (\Space{..} -> spaceName == "S") <$> S.all base
      Id.isPermanent s' `shouldBe` True

      Just p' <- find (\Property{..} -> propertyName == "P") <$> S.all base
      Id.isPermanent p' `shouldBe` True

      Just t' <- find (\Theorem{..} -> theoremDescription == "T") <$> S.all base
      Id.isPermanent (t' :: Theorem PropertyId) `shouldBe` True

      Just t1' <- Trait.find base (spaceId s') (propertyId p')
      _traitValue t1' `shouldBe` True

      Just t2' <- Trait.find base (spaceId s') compact
      _traitValue t2' `shouldBe` False
