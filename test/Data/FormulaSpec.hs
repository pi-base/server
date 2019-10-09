module Data.FormulaSpec (spec) where

import Test.Import hiding (negate)

import Data.Formula

import qualified Data.Set  as Set
import qualified Data.Text as Text

compact, connected, t2 :: Text
compact   = "compact"
connected = "connected"
t2        = "t2"

spec :: IO TestTree
spec = testSpec "Data.Formula" $ parallel $ do
  let
    atom     = compact .= True
    compound = (compact .= True) \/ (connected .= False /\ t2 .= True)

  describe "fmap" $ do
    it "maps over properties" $ do
      fmap Text.length compound `shouldBe` (7 .= True) \/ (9 .= False /\ 2 .= True)

  describe "/\\" $ do
    it "flattens constructors" $ do
      compact .= True /\ connected .= True /\ t2 .= True `shouldBe`
        And [ compact .= True, connected .= True, t2 .= True ]

  describe "format" $ do
    it "can display negations" $ do
      let f = compact .= False
      format identity f `shouldBe` "~compact"

    it "can display compound formulae" $ do
      format identity compound `shouldBe` "(compact | (~connected + t2))"

  describe "negate" $ do
    it "should negate" $ do
      negate (compact .= True) `shouldBe` (compact .= False)

  describe "parse" $ do
    it "can parse true" $ do
      parse "compact" `shouldBe` Right (compact .= True)

    it "can parse false" $ do
      parse "~compact" `shouldBe` Right (compact .= False)

    it "can parse conjunctions with parens" $ do
      parse "(a + ~b)" `shouldBe` Right ("a" .= True /\ "b" .= False)

    it "can parse conjunctions without parens" $ do
      parse "a + b + c" `shouldBe` Right ("a" .= True /\ "b" .= True /\ "c" .= True)

    it "can parse nested conjunctions and disjunctions" $ do
      parse "a + (b | ~c) + d" `shouldBe` Right
        ("a" .= True /\ ("b" .= True \/ "c" .= False) /\ "d" .= True)

  describe "properties" $ do
    it "can list for atoms" $ do
      properties atom `shouldBe` Set.singleton "compact"

    it "can list for compounds" $ do
      properties compound `shouldBe` Set.fromList ["compact", "connected", "t2"]
