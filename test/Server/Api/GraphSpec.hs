module Server.Api.GraphSpec (spec) where

import Test.Server.Import

import Data.Aeson.Lens

import qualified Data.Text as T

spec :: Runner -> TestM TestTree
spec run = do
  app <- makeApp run

  specify "Server.Api.Graph" $ before (run reset) $ do
    describe "/graph" $ do
      it "can perform queries" $ run $ do
        Response{..} <- post app "/graphql" [json|{
          "operationName": "Viewer"
        }|]

        status `shouldBe` 200

        let spaces = body ^. key "data" . key "viewer" . key "spaces" . _Array
        length spaces `shouldSatisfy` (>= 130)

      it "can perform authenticated queries" $ run $ do
        login testUser

        Response{..} <- post app "/graphql" [json|{
          "operationName": "Me"
        }|]

        status `shouldBe` 200

        body ^. key "data" . key "me" . key "name" . _String `shouldBe` "test"

        let branches = body ^. key "data" . key "me" . key "branches" . _Array
        length branches `shouldSatisfy` (>= 2)

      it "can perform mutations" $ run $ do
        login testUser

        Response{..} <- post app "/graphql" [json|{
          "operationName": "CreateSpace",
          "variables": {
            "patch": {
              "branch": "users/test@example.com",
              "sha": #{headSha}
            },
            "space": {
              "name": "New Space"
            }
          }
        }|]

        status `shouldBe` 200
        body ^. key "data" . key "createSpace" . key "spaces" . nth 0 . key "name" . _String `shouldBe` "New Space"

      it "can require login" $ run $ do
        Response{..} <- post app "/graphql" [json|{
          "operationName": "CreateSpace",
          "variables": {
            "patch": {
              "branch": "users/test@example.com",
              "sha": #{headSha}
            },
            "space": {
              "name": "New Space"
            }
          }
        }|]

        status `shouldBe` 401
        body `shouldBe` [json|{
          "error": true,
          "message": "Unauthorized"
        }|]

    describe "/graphql/schema" $ do
      it "shows the schema" $ run $ do
        Response{..} <- get app "/graphql/schema"
        status `shouldBe` 200
        T.unpack (encodeText body) `shouldInclude` "schema"
