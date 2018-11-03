module Services.GithubSpec (spec) where

import Test.Import

import Services.Github as Github

spec :: Runner -> TestM TestTree
spec run = specify "Services.Github" $ do
  describe "user" $ do
    it "can fetch a user" $ run $ do
      stub "https://api.github.com/user" [json|{
        "id": 123,
        "name": "Test",
        "email": "test@example.com"
      }|]

      Just Github.User{..} <- Github.user "token"

      ghUserId `shouldBe` 123
      ghUserName `shouldBe` "Test"
      ghUserEmail `shouldBe` "test@example.com"

    it "can fail to find a user" $ run $ do
      stub "https://api.github.com/user" [json|{
        "message": "Requires authentication"
      }|]

      muser <- Github.user "token"
      muser `shouldBe` Nothing
