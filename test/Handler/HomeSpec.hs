module Handler.HomeSpec (spec) where

import TestImport

spec :: IO (TestApp App) -> IO TestTree
spec getApp = testSpec "Handler.HomeSpec" $ do
  before getApp $
    describe "Homepage" $ do
      it "loads the index and checks it looks right" $ do
        get HomeR

        json $ \j -> do
          j `shouldHaveKey` "HEAD"
        statusIs 200