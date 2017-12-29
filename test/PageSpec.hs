{-# OPTIONS_GHC -fno-warn-orphans #-}
module PageSpec (spec) where

import TestImport

import Test.QuickCheck

import qualified Data.Text as T

import Core
import Page
import qualified Page.Property as Property
import qualified Page.Space    as Space
import qualified Page.Theorem  as Theorem
import qualified Page.Trait    as Trait

instance Arbitrary Uid where
  arbitrary = arbitrary

instance Arbitrary (Id a) where
  arbitrary = do
    n <- arbitrary
    return . Id $ tshow (n :: Int)

-- TODO: these description instances should be arbitrary,
--   but there are a few restrictions on what's valid
instance Arbitrary Core.Property where
  arbitrary = Property
    <$> arbitrary
    <*> string
    <*> string
    <*> pure []
    <*> pure "description"

instance Arbitrary Space where
  arbitrary = Space
    <$> arbitrary
    <*> string
    <*> string
    <*> pure []
    <*> pure "description"
    <*> pure Nothing

instance Arbitrary p => Arbitrary (Theorem p) where
  arbitrary = Theorem
    <$> arbitrary
    <*> arbitrary
    <*> pure Nothing
    <*> pure "description"

instance (Arbitrary s, Arbitrary p) => Arbitrary (Trait s p) where
  arbitrary = Trait
    <$> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> pure "description"

instance Arbitrary p => Arbitrary (Implication p) where
  arbitrary = Implication <$> arbitrary <*> arbitrary

instance Arbitrary p => Arbitrary (Formula p) where
  arbitrary = elements ([1,2,3] :: [Int]) >>= gen
    where
      gen 1 = do
        n <- choose (2, 4)
        And <$> replicateM n (gen 3)
      gen 2 = do
        n <- choose (2, 4)
        Or <$> replicateM n (gen 3)
      gen _ = Atom <$> arbitrary <*> arbitrary

string :: Gen Text
string = fmap T.pack arbitrary

invertable :: (Eq a, Show a, Arbitrary a)
           => Page a -> Test.QuickCheck.Property
invertable page = property $ \obj -> (parse page $ write page obj) == Right obj

spec :: IO TestTree
spec = testSpec "PageSpec" $ do
  describe "Parsing pages" $ do
    it "property is invertable" $
      invertable $ Property.page

    it "space is invertable" $
      invertable $ Space.page

    it "theorem is invertable" $
      invertable $ Theorem.page

    it "trait is invertable" $
      invertable $ Trait.page

