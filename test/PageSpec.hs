module PageSpec (spec) where

import           Test.Import
import qualified Test.QuickCheck

import           Page
import qualified Page.Property as Property
import qualified Page.Space    as Space
import qualified Page.Theorem  as Theorem
import qualified Page.Trait    as Trait

invertable :: (Eq a, Show a, Arbitrary a)
           => Page a -> Test.QuickCheck.Property
invertable page = property $ \obj -> (parse page $ write page obj) == Right obj

spec :: TestM TestTree
spec = specify "Page" $ do
  describe "Parsing pages" $ do
    it "property is invertable" $
      invertable $ Property.page

    it "space is invertable" $
      invertable $ Space.page

    it "theorem is invertable" $
      invertable $ Theorem.page

    it "trait is invertable" $
      invertable $ Trait.page
