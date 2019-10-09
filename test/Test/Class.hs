{-# OPTIONS_GHC -fno-warn-orphans #-}
module Test.Class where

import Core

import qualified Data.Text        as T
import qualified Data.Trait       as Trait
import qualified Data.UUID.V4     as UUID
import           Test.QuickCheck
import           System.IO.Unsafe (unsafePerformIO)

instance Arbitrary UUID where
  arbitrary = return $ unsafePerformIO UUID.nextRandom

instance Arbitrary (Id a) where
  arbitrary = arbitrary >>= \heads ->
    if heads
      then CanonicalId . positive <$> arbitrary
      else TemporaryId <$> arbitrary

positive :: Int -> Int
positive n
  | n == 0 = 1
  | n > 0  = 2 * n
  | otherwise = 2 * (-n) + 1

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

instance Arbitrary Theorem where
  arbitrary = Theorem
    <$> arbitrary
    <*> arbitrary
    <*> pure Nothing
    <*> pure "description"
    <*> pure []

instance Arbitrary Trait where
  arbitrary = Trait
    <$> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> pure "description"
    <*> pure []

instance Arbitrary Trait.Value where
  arbitrary = Trait.Value <$> arbitrary

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
      gen _ = Atom
        <$> arbitrary
        <*> (Trait.Value <$> arbitrary)

string :: Gen Text
string = fmap T.pack arbitrary
