{-# OPTIONS_GHC -fno-warn-orphans #-}
module PageSpec (spec) where

import TestImport

import Test.QuickCheck

import qualified Data.Text as T

import Core
import Page
import qualified Page.Property as Property
import qualified Page.Space    as Space
import qualified Page.Trait    as Trait
import qualified Page.Theorem  as Theorem

instance Arbitrary Uid where
  arbitrary = arbitrary

instance Arbitrary PropertyId where
  arbitrary = do
    n <- arbitrary
    return . PropertyId $ tshow (n :: Int)

instance Arbitrary Core.Property where
  arbitrary = Property
    <$> arbitrary
    <*> string
    <*> string
    <*> pure Nothing
    <*> pure "description"

instance Arbitrary SpaceId where
  arbitrary = do
    n <- arbitrary
    return . SpaceId $ tshow (n :: Int)

instance Arbitrary Space where
  arbitrary = Space
    <$> arbitrary
    <*> string
    <*> string
    <*> pure "desc"
    <*> pure Nothing

string = fmap T.pack arbitrary

invertable :: (Eq a, Show a, Arbitrary a)
           => Page a -> Test.QuickCheck.Property
invertable page = property $ \obj -> (parse page $ write page obj) == Right obj

spec :: Spec
spec = describe "Parsing pages" $ do
  it "property is invertable" $
    invertable $ Property.page

  it "space is invertable" $
    invertable $ Space.page

  -- Also TODO - arbitrary instances should generate descriptions, but there are
  --   _some_ rules on them (e.g. can't contain section dividers)
  xit "trait is invertable"   $ property $ \x -> 1 == (x :: Int)
  xit "theorem is invertable" $ property $ \x -> 1 == (x :: Int)
