module Persist.Backend.Git.PageSpec (spec) where

import Test.Import hiding (Spec)

import Persist.Backend.Git.Page

import           Data.Aeson          (toJSON)
import qualified Data.HashMap.Strict as HM
import           Data.Structure      (HKD)

data Result' f = Result
  { resultInt   :: HKD f Int
  , resultText  :: HKD f Text
  } deriving Generic

data MonoidResult' f = MonoidResult
  { resultList :: HKD f [Int]
  } deriving Generic

data SectionResult' f = SectionResult
  { result1 :: HKD f (Maybe Text)
  , result2 :: HKD f (Maybe Text)
  } deriving Generic

type Result        = Result' Identity
type MonoidResult  = MonoidResult' Identity
type SectionResult = SectionResult' Identity

deriving instance Show Result
deriving instance Eq   Result
deriving instance Show MonoidResult
deriving instance Eq   MonoidResult
deriving instance Show SectionResult
deriving instance Eq   SectionResult

inHeader :: Result' Field
inHeader = Result
  (header "int")
  (header "text")

inMain :: Result' Field
inMain = Result
  (header "int")
  main

withList :: MonoidResult' Field
withList = MonoidResult
  (header' "list")

withSections :: SectionResult' Field
withSections = SectionResult
  (section "first")
  (section "second")

spec :: IO TestTree
spec = testSpec "Persist.Backend.Git.Page" $ parallel $ do
  describe "parse" $ do
    it "can parse in the header" $ do
      let
        sheet = Data
          { _headers = HM.fromList
            [ ("text", toJSON ("hunter" :: Text))
            , ("int",  toJSON (2 :: Int))
            ]
          , _main = ""
          , _sections = mempty
          }
      parse' inHeader sheet `shouldBe` Just (Result 2 "hunter")

    it "can parse in main" $ do
      let
        sheet = Data
          { _headers = HM.fromList
            [ ("text", toJSON ("hunter" :: Text))
            , ("int",  toJSON (1 :: Int))
            ]
          , _main = "main"
          , _sections = mempty
          }
      parse' inMain sheet `shouldBe` Just (Result 1 "main")

    it "can parse monoids" $ do
      let
        sheet = Data
          { _headers = HM.fromList
            [ ("list", toJSON ([1, 2, 3] :: [Int]))
            ]
          , _main = ""
          , _sections = mempty
          }
      parse' withList sheet `shouldBe` Just (MonoidResult [1, 2, 3])

    it "can parse monoids with default values" $ do
      let
        sheet = Data
          { _headers = mempty
          , _main = ""
          , _sections = mempty
          }
      parse' withList sheet `shouldBe` Just (MonoidResult [])

    it "can parse sections" $ do
      let
        sheet = Data
          { _headers = mempty
          , _main = ""
          , _sections = HM.fromList
            [ ("first", "first")
            , ("second", "second")
            ]
          }
      parse' withSections sheet `shouldBe` Just (SectionResult (Just "first") (Just "second"))

    it "can fail to find sections" $ do
      let
        sheet = Data
          { _headers = mempty
          , _main = ""
          , _sections = HM.fromList
            [ ("second", "second")
            ]
          }
      parse' withSections sheet `shouldBe` Just (SectionResult Nothing (Just "second"))
