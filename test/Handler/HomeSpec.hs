module Handler.HomeSpec (spec) where

import TestImport

spec :: IO TestTree
spec = testSpec "Handler.HomeSpec" $ do
  withApp $
    describe "Homepage" $ do
      it "loads the index and checks it looks right" $ do
        get HomeR

        json $ \j -> do
          j `shouldHaveKey` "version"
        statusIs 200