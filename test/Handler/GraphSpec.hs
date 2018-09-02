module Handler.GraphSpec (spec) where

import TestImport

import           Control.Lens         hiding ((.=))
import           Data.Aeson
import           Data.Aeson.Lens
import qualified Data.Aeson.Types     as Aeson
import qualified Data.ByteString.Lazy as LBS
import           Network.HTTP.Types.Method

import Handler.Helpers (ensureUser, ensureToken)

testUser :: User
testUser = User "github:5678" "graphtest" "graphtest@example.com" "github-token-xxx" False

spec :: IO (TestApp App) -> IO TestTree
spec getApp = do
  app@(foundation, _) <- getApp

  let
    run :: Handler a -> IO a
    run = unsafeHandler foundation

    setup :: IO (TestApp App)
    setup = run $ return app

    reset :: IO ()
    reset = run $ return ()

    as :: User -> YesodExample App Text
    as user = do
      let token = "test:" <> userIdent user
      -- TODO: there's probably a more direct way to run
      -- a handler inside this monad
      void . liftIO . run $ do
        (Entity _id _) <- ensureUser user
        ensureToken _id token
      return token

  testSpec "Handler.GraphSpec" $ do
    beforeAll setup $ before_ reset $ do
      describe "Queries" $ do
        it "can view user info" $ do
          token <- as testUser

          d <- query token "{ me { name } }"

          assertEq "user name" (userName testUser) $
            d ^. key "me" . key "name" . _String

send :: Text -> [Aeson.Pair] -> YesodExample App ()
send token body = request $ do
  setUrl           GraphR
  setMethod        methodPost
  addRequestHeader ("Authorization", "Bearer " <> encodeUtf8 token)
  setRequestBody . encode $ object body

query :: Text -> Text -> YesodExample App LBS.ByteString
query token q = do
  send token
    [ "operationName" .= ("" :: Text)
    , "query"         .= q
    ]
  body <- checkResponse
  return $ encode $ body ^. key "data" . _Object

checkResponse :: SIO (YesodExampleData site) LBS.ByteString
checkResponse = do
  body <- getResponseBody
  assertEq "errors" [] $ body ^.. key "errors" . values . key "message" . _String
  statusIs 200
  return body
