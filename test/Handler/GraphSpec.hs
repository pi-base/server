module Handler.GraphSpec (spec) where

import TestImport

import           Control.Lens         hiding ((.=))
import           Data.Aeson
import           Data.Aeson.Lens
import qualified Data.Aeson.Types     as Aeson
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text            as T
import           Network.HTTP.Types.Method

initialVersion :: Text
initialVersion = "63718fe2f72bf355e8c17b37bc6be7b631604aaa"

testToken :: IsString a => a
testToken = "test-token"

testUser :: User
testUser = User "test" "test" "test@example.com" "github-token-xxx"

spec :: IO TestTree
spec = do
  app@(foundation, _) <- buildApp

  let setup :: IO (TestApp App)
      setup = unsafeHandler foundation $ return app

  let reset :: IO ()
      reset = void $ unsafeHandler foundation $ return ()

  testSpec "Handler.GraphSpec" $ do
    beforeAll setup $ before_ reset $ do
      xdescribe "Queries" $ do
        it "can view user info" $ do
          d <- query "{ me { name } }"

          assertEq "user name" (userName testUser) $
            d ^. key "me" . key "name" . _String

send :: [Aeson.Pair] -> YesodExample App ()
send body = request $ do
  setUrl           GraphR
  setMethod        methodPost
  addRequestHeader ("Authorization", "Bearer " <> testToken)
  setRequestBody . encode $ object body

query :: Text -> YesodExample App LBS.ByteString
query q = do
  send [ "operationName" .= ("" :: Text)
       , "query"         .= q
       ]
  body <- checkResponse
  return $ encode $ body ^. key "data" . _Object

mutation :: Text -> Text -> [Aeson.Pair] -> YesodExample App LBS.ByteString
mutation name fields input = do
  let q = T.unlines [ "mutation " <> name <> "($input: MutationInput!) {"
                    , "  " <> name <> "(input: $input) " <> fields
                    , "}"
                    ]
  send [ "operationName" .= ("" :: Text)
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
