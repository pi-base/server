module Handler.HomeSpec (spec) where

import TestImport

spec :: Spec
spec = withApp $
  describe "Homepage" $ do
    xit "loads the index and checks it looks right" $ do
      get HomeR

      statusIs 200
      json $ \j -> do
        j `shouldHaveKey` "version"

    xit "leaves the user table empty" $ do
      get HomeR

      statusIs 200
      users <- runDB $ selectList ([] :: [Filter User]) []
      assertEq "user table empty" 0 $ length users
