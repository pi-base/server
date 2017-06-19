module Handler.GraphSpec (spec) where

import TestImport

import           Control.Lens         hiding ((.=))
import           Data.Aeson
import           Data.Aeson.Lens
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text            as T
import           Network.HTTP.Types.Method

import Data.Git        (resetRef, userBranch, useRepo)
import Handler.Helpers (attachToken)
import Types           (Committish(..))

initialVersion :: Text
initialVersion = "2496bccf4c23c018c31bf57045518030588993d5"

testToken :: IsString a => a
testToken = "test-token"

testUser :: User
testUser = User "test" "test" "test@example.com" "github-token-xxx"

spec :: Spec
spec = do
  app@(foundation, _) <- runIO buildApp

  let setup :: IO (TestApp App)
      setup = unsafeHandler foundation $ do
        userId <- createGithubUser testUser
        _ <- attachToken userId testToken
        return app

  let reset :: IO ()
      reset = unsafeHandler foundation $
        useRepo $
          resetRef (userBranch testUser) (CommitSha initialVersion)

  beforeAll setup $ before_ reset $ describe "Queries" $ do

    it "can fetch properties" $ do
      d <- query "{ viewer { version, properties { uid, name } } }"

      assertEq "version" initialVersion $
        d ^. key "viewer" . key "version" . _String
      assertEq "property count" 100 $
        length $ d ^.. key "viewer" . key "properties" . values . key "uid" . _String


    it "can view user info" $ do
      d <- query "{ me { name } }"

      assertEq "user name" (userName testUser) $
        d ^. key "me" . key "name" . _String


    it "can add a space" $ do
      d <- mutation "createSpace" "{ version, spaces { uid, name, description } }"
             [ "name"        .= ("New Space" :: Text)
             , "description" .= ("New space description" :: Text)
             ]

      assertEq "space names" ["New Space"] $
        d ^.. key "spaces" . values . key "name" . _String
      assertNotEq "version" initialVersion $
        d ^. key "version" . _String


    it "can add a property" $ do
      d <- mutation "createProperty" "{ version, properties { uid, name, description } }"
             [ "name"        .= ("New Property" :: Text)
             , "description" .= ("New property description" :: Text)
             ]

      assertEq "property names" ["New Property"] $
        d ^.. key "properties" . values . key "name"
      assertNotEq "version" initialVersion $
        d ^. key "version" . _String


    xit "can assert a trait" $ do
      s <- mutation "createSpace" "{ spaces { uid } }"
             [ "name"        .= ("S" :: Text)
             , "description" .= ("" :: Text)
             ]

      let sid = s ^. key "spaces" . nth 0 . key "uid" . _String
          compact    = "P000016" :: Text
          metrizable = "P000053" :: Text
          q = "{ version, spaces { name, traits { property { name } value } } }"

      -- S |= compact
      t1 <- mutation "assertTrait" q
              [ "spaceId"     .= sid
              , "propertyId"  .= compact
              , "value"       .= True
              , "description" .= ("" :: Text)
              ]

      assertNotEq "version" initialVersion $
        t1 ^. key "version" . _String
      assertEq "spaces" ["S"] $
        t1 ^.. key "spaces" . values . key "name" . _String

      -- TODO:
      -- traits include compact = true and countably compact = true
      -- > 3 traits
      -- all traits true
      assertEq "derived new traits" (take 19 $ repeat True) $
        t1 ^.. key "spaces" . nth 0 . key "traits" . values . key "value" . _Bool

      -- S |= ~metrizable
      t2 <- mutation "assertTrait" q
              [ "spaceId"     .= sid
              , "propertyId"  .= metrizable
              , "value"       .= False
              , "description" .= ("" :: Text)
              ]
      print t2

      assertNotEq "version" initialVersion $
        t2 ^. key "version" . _String
      assertEq "spaces" ["S"] $
        t2 ^.. key "spaces" . values . key "name" . _String

      -- TODO:
      -- assertion about derived traits

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

-- send :: (RedirectUrl site (Route App), Yesod site)
--      => [Aeson.Pair] -> YesodExample site ()
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

-- mutation :: ToJSON a => Text -> Text -> a -> YesodExample App LBS.ByteString
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
