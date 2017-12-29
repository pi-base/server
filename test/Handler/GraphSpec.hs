module Handler.GraphSpec (spec) where

import TestImport

import           Control.Lens         hiding ((.=))
import           Data.Aeson
import           Data.Aeson.Lens
import qualified Data.Aeson.Types     as Aeson
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text            as T
import           Network.HTTP.Types.Method

import Handler.Helpers (ensureUser, ensureToken)

initialVersion :: Text
initialVersion = "63718fe2f72bf355e8c17b37bc6be7b631604aaa"

testUser :: User
testUser = User "graphtest" "Graph Test" "graphtest@example.com" "github-token-xxx"

spec :: IO TestTree
spec = do
  app@(foundation, _) <- buildApp

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

mutation :: Text -> Text -> Text -> [Aeson.Pair] -> YesodExample App LBS.ByteString
mutation token name fields input = do
  let q = T.unlines [ "mutation " <> name <> "($input: MutationInput!) {"
                    , "  " <> name <> "(input: $input) " <> fields
                    , "}"
                    ]
  send token
    [ "operationName" .= ("" :: Text)
    , "query"         .= q
    , "variables"     .= object
      [ "input" .= object input ]
    ]
  body <- checkResponse
  return $ encode $ body ^. key "data" . key name . _Object

checkResponse :: StateT (YesodExampleData site) IO LBS.ByteString
checkResponse = do
  body <- getResponseBody
  assertEq "errors" [] $ body ^.. key "errors" . values . key "message" . _String
  statusIs 200
  return body
