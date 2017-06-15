module Handler.GraphSpec (spec) where

import TestImport

import           Control.Lens
import qualified Data.Aeson           as A
import           Data.Aeson.Lens
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text            as T
import           Network.HTTP.Types.Method

send token body = request $ do
  setUrl           GraphR
  setMethod        methodPost
  addRequestHeader ("Authorization", "Bearer " <> token)
  setRequestBody . A.encode $ A.object body

query :: ByteString -> Text -> YesodExample App LBS.ByteString
query token q = do
  send token [ "operationName" A..= ("" :: Text)
             , "query"         A..= q
             ]
  -- TODO: only include `data`
  checkResponse

mutation :: A.ToJSON a => ByteString -> Text -> Text -> a -> YesodExample App LBS.ByteString
mutation token name fields input = do
  let q = T.unlines [ "mutation " <> name <> "($input: MutationInput!) {"
                    , "  " <> name <> "(input: $input) " <> fields
                    , "}"
                    ]
  send token [ "operationName" A..= ("" :: Text)
             , "query"         A..= q
             , "variables"     A..= A.object
               [ "input" A..= input ]
             ]
  -- TODO: only include name
  checkResponse

checkResponse :: StateT (YesodExampleData site) IO LBS.ByteString
checkResponse = do
  body <- getResponseBody
  assertEq "errors" [] $ body ^.. key "errors" . values . key "message" . _String
  statusIs 200
  return body

resetTo hash = putStrLn "TODO: setup user / token; reset git repo to commit"

spec :: Spec
spec = withApp $ do
  let token = "test-token"
      initialVersion = "a618508e0e65b1bcd59e01ee70ab0d5f021ec5b5"

  before_ (resetTo initialVersion) $ describe "Queries" $ do

    it "can fetch properties" $ do
      d <- query token "{ viewer { version, properties { uid, name } } }"

      assertEq "version" initialVersion $
        d ^. key "data" . key "viewer" . key "version" . _String
      assertEq "property count" 100 $
        length $ d ^.. key "data" . key "viewer" . key "properties" . values . key "uid" . _String

    it "can view user info" $ do
      createUser "test" token

      d <- query token "{ me { name } }"
      assertEq "user name" "test" $
        d ^. key "data" . key "me" . key "name" . _String

    it "can add a space" $ do
      createUser "test" token

      let input = A.object [ "name"        A..= ("New Space" :: Text)
                           , "description" A..= ("New space description" :: Text)
                           ]

      d <- mutation token "createSpace" "{ version, spaces { uid, name, description } }" input

      -- version != old version
      assertEq "space names" ["New Space"] $
        d ^.. key "data" . key "createSpace" . key "spaces" . values . key "name"

    -- xit "can add a property" $ do
    --   -- version is new
    --   -- 1 property in response
    --      -- name is ...
    --      -- uid starts with p
    --   return ()

    -- xit "can assert a trait" $ do
    --   -- create space S
    --   -- assert S |= compact
    --   -- version is new
    --   -- only space in response is S
    --      -- traits include compact = true and countably compact = true
    --      -- > 3 traits
    --   return ()

    -- xit "can assert a theorem" $ do
    --   -- create property P
    --   -- assert compact => P
    --   -- version is new
    --   -- 1 theorem in response
    --   -- 40 .. 100 spaces in response
    --   -- each space
    --      -- name present
    --      -- traits.count = 1
    --         -- trait property is P
    --         -- trait value is true
    --   -- also assert P => paracompact?
    --   return ()
