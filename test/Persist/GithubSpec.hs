module Persist.GithubSpec (spec) where

import Test.Import

import Persist.Github

import           Data.Aeson
import qualified Persist.Http as Http

config :: Config
config = Config
  { credentials = OAuth2 "" "" ""
    , token = ""
    , repo = "server:test"
    , owner = "pi-base"
  }

spec :: IO TestTree
spec = testSpec "Persist.Github" $ do
  let
    run :: [Value] -> Sem '[Github, Http.Http, Expect, Fail, Embed IO] a -> IO a
    run values action = runHttp config action
      & Http.runList values
      & runTest

  describe "pullRequests" $ do
    it "is empty by default" $ do
      run [] $ do
        [] <== pullRequests

    it "can return a list" $ do
      let
        urls = map (\i -> "http://example.com/pulls/" <> show i) ([1 .. 3] :: [Int])
        response = toJSON $ map (\url -> object [ "html_url" .= (url :: Text)]) urls

      run [response] $ do
        map PullRequest urls <== pullRequests

    -- TODO: openPullRequest
