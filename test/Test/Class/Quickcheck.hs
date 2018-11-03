{-# OPTIONS_GHC -fno-warn-orphans #-}
module Test.Class.Quickcheck where

import Core

import Test.QuickCheck

import qualified Data.Text as T

instance Arbitrary Uid where
  arbitrary = arbitrary

instance Arbitrary (Id a) where
  arbitrary = do
    n <- arbitrary
    return . Id $ show (n :: Int)

-- TODO: these description instances should be arbitrary,
--   but there are a few restrictions on what's valid
instance Arbitrary Core.Property where
  arbitrary = Property
    <$> arbitrary
    <*> string
    <*> pure []
    <*> pure "description"
    <*> pure []

instance Arbitrary Space where
  arbitrary = Space
    <$> arbitrary
    <*> string
    <*> pure []
    <*> pure "description"
    <*> pure Nothing
    <*> pure []

instance Arbitrary p => Arbitrary (Theorem p) where
  arbitrary = Theorem
    <$> arbitrary
    <*> arbitrary
    <*> pure Nothing
    <*> pure "description"
    <*> pure []

instance (Arbitrary s, Arbitrary p) => Arbitrary (Trait s p) where
  arbitrary = Trait
    <$> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> pure []
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
