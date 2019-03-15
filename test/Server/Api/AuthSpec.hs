module Server.Api.AuthSpec (spec) where

import Test.Server.Import hiding (takeWhile)

import           Data.Attoparsec.Text
import qualified Data.Text            as T

import qualified Auth

spec :: Runner -> TestM TestTree
spec run = do
  app <- makeApp run

  specify "Server.Api.Auth" $ do
    describe "/auth/github" $ do
      it "redirects to Github" $ run $ do
        res@Response{..} <- get app "/auth/github"

        status `shouldBe` 303
        header res "Location" `shouldStartWith` "https://github.com/login/oauth/authorize"

    describe "/auth/github/callback" $ do
      it "can create a new user" $ run $ do
        stub "https://github.com/login/oauth/access_token" [json|{
          "access_token": "XXX"
        }|]

        stub "https://api.github.com/user" [json|{
          "id": 123,
          "login": "test-user",
          "name": "Create Test",
          "email": "create.test@example.com"
        }|]

        res@Response{..} <- get app "/auth/github/callback?code=test"
        status `shouldBe` 303

        let Right token = parseOnly
              ("https://viewer.example.com/login/" *> takeWhile (/= '/') <* endOfInput)
              (T.pack $ header res "Location")

        Just (Entity _ User{..}) <- Auth.userWithToken token
        userName `shouldBe` "Create Test"
        userEmail `shouldBe` "create.test@example.com"

      it "expects a token" $ run $ do
        res@Response{..} <- get app "/auth/github/callback"

        status `shouldBe` 303

        let location = header res "Location"
        location `shouldStartWith` "https://viewer.example.com"
        location `shouldInclude` "error=Did not receive a login code"
