module Server.ApiSpec (spec) where

import Test.Server.Import

import qualified Build
import           Control.Lens (view)
import qualified Data.Git     as Git

spec :: Runner -> TestM TestTree
spec run = do
  app <- makeApp run
  env <- view foundation

  sha <- Git.headSha "test"

  specify "Server.Api" $ do
    describe "/" $ do
      it "shows metadata" $ do
        Response{..} <- run $ get app "/"

        status `shouldBe` 200
        body `shouldBe` [json|{
          "data": {
            "head": #{sha}
          },
          "build": #{Build.info},
          "startedAt": #{env ^. envBootTime}
        }|]

    describe "/app" $ do
      it "redirects to the viewer" $ do
        res@Response{..} <- run $ get app "/app"

        status `shouldBe` 303
        header res "Location" `shouldBe` "https://viewer.example.com"
