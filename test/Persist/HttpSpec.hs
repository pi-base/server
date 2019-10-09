module Persist.HttpSpec (spec) where

import Test.Import

import Persist.Http

import Data.Aeson

spec :: IO TestTree
spec = testSpec "Persist.Http" $ parallel $ do
  describe "with mock responses" $ do
    let
      foobar :: Value
      foobar = object [ "foo" .= ("bar" :: Text)]

      run :: [Value] -> Sem '[Http, Expect, Embed IO] a -> IO a
      run values = runM . check . runList values

    it "can stub a response" $ run (repeat foobar) $ do
      foobar <== get  defaults ""
      foobar <== post defaults "" (toJSON $ object [])

    it "returns Null when no response is stubbed" $ run [] $ do
      Null <== get defaults ""
