module Data.IdSpec (spec) where

import           Test.Import
-- import qualified Test.QuickCheck as Q

import           Data.Id
import qualified Data.UUID as UUID

spec :: IO TestTree
spec = testSpec "Data.Id" $ parallel $ do
  let
    uuidT     = "c2cc10e1-57d6-4b6f-9899-38d972112d8c"
    Just uuid = UUID.fromText uuidT

  describe "format" $ do
    it "can format a canonical id" $ do
      format 'S' (fromInt 123) `shouldBe` "S000123"

    it "can format a temporary id" $ do
      format 'S' (TemporaryId uuid) `shouldBe` "s" <> uuidT

  describe "parse" $ do
    it "can parse a canonical id" $ do
      parse "P" "P0012" `shouldBe` Right (CanonicalId 12)

    it "can parse a temporary id" $ do
      parse "P" ("p" <> uuidT) `shouldBe` Right (TemporaryId uuid)

    it "does not parse if the prefix does not match" $ do
      parse "T" "P0012" `shouldBe` Left "Failed reading: satisfy"

    -- TODO
    -- it "inverts format" $ do
    --   Q.property $ \(Letter char, id) ->
    --     (parse [char] $ format char id) == Just id
