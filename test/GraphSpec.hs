module GraphSpec (spec) where

import TestImport (App(..), buildApp)
import Test.Tasty
import Test.Tasty.Hspec

import           Control.Monad.Logger     (MonadLogger(..), runStdoutLoggingT)
import           Control.Lens             hiding ((.=))
import           Data.Aeson               (Value(..), (.=), object)
import           Data.Aeson.Encode.Pretty (encodePretty)
import           Data.Aeson.Lens
import qualified Data.HashMap.Strict      as HM
import qualified Data.Map                 as M
import qualified Data.Text                as T
import           Database.Persist.Sql     (ConnectionPool, Entity(..), runSqlPool, toSqlKey)
import           Git.Libgit2              (HasLgRepo(..))

import qualified Data.ByteString.Lazy.Char8 as BS

import Core
import Data.Store    (storeRepo)
import Graph.Queries (compileAll)
import Graph.Root    (exec)
import Graph.Types   (Query)

data Config = Config
  { pool   :: ConnectionPool
  , store  :: Store
  , user   :: User
  }

instance MonadDB (ReaderT Config IO) where
  db action = do
    conn <- asks pool
    runSqlPool action $ conn
instance HasLgRepo (ReaderT Config IO) where
  getRepository = asks $ storeRepo . store
instance MonadStore (ReaderT Config IO) where
  getStore = asks store
instance MonadGraph (ReaderT Config IO) where
  -- TODO: probably should insert user into DB lazily
  requireUser = do
    u <- asks user
    return $ Entity (toSqlKey (-1)) u
instance MonadLogger IO where
  monadLoggerLog _ _ _ _ = return () -- FIXME


testUser :: User
testUser = User "test" "Test User" "test@example.com" "xxx"

pp :: (ToJSON a, Monad m) => a -> m ()
pp = traceM . BS.unpack . encodePretty

v = key "data" . key "viewer"

compact, metacompact, metrizable :: Text
compact     = "P000016"
metacompact = "P000031"
metrizable  = "P000053"

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _ = False

spec :: IO TestTree
spec = do
  -- NEXT:
  -- * rework test auto-runner
  -- * move most specs from Handler/GraphSpecs to here
  -- * re-enable and spec mutations
  (App{..}, _) <- buildApp
  queries <- compileAll

  let
    config = Config
      { pool   = appConnPool
      , store  = appStore
      , user   = testUser
      }

    run :: Query -> [(Text, Value)] -> IO Value
    run q vars = runReaderT (exec q (Just $ HM.fromList vars)) config

    query :: FilePath -> [(Text, Value)] -> IO Value
    query name vars = do
      let (Just (Right compiled)) = M.lookup ("graph" </> "queries" </> name <> ".gql") queries
      result <- run compiled vars
      return . Object $ result ^. key "data" . _Object

    mutation :: FilePath -> [(Text, Value)] -> IO Value
    mutation name vars = do
      let (Just (Right compiled)) = M.lookup ("graph" </> "mutations" </> name <> ".gql") queries
      result <- run compiled vars
      return . Object $ result ^. key "data" . _Object

  testSpec "graph" $ do
    describe "query compilation" $ do
      it "compiles all queries" $ do
        M.filter isLeft queries `shouldBe` mempty

    describe "user" $ do
      it "can fetch user information" $ do
        user <- query "me" mempty

        user ^.  key "me" . key "name" . _String `shouldBe` "Test User"
        user ^.. key "me" . key "branches" . values . key "name" . _String `shouldBe` ["master", "users/test"]

    describe "viewer" $ do
      it "can run queries directly" $ do
        result <- query "viewer" mempty

        let spaceIds = result ^.. key "viewer" . key "spaces" . values . key "uid" . _String
        length spaceIds `shouldBe` 160

        let propIds = result ^.. key "viewer" . key "properties" . values . key "uid" . _String
        length propIds `shouldBe` 100

        let theoremIds = result ^.. key "viewer" . key "theorems" . values . key "uid" . _String
        length theoremIds `shouldBe` 226

      it "can add a space" $ do
        result <- mutation "createSpace" $
                    [ "patch" .= object
                      [ "branch" .= ("test" :: Text)
                      , "sha"    .= ("123" :: Text)
                      ]
                    , "input" .= object
                      [ "name"        .= ("New Space" :: Text)
                      , "description" .= ("Desc" :: Text)
                      ]
                    ]

        let names = result ^.. key "patch" . key "createSpace" . key "spaces" . values . key "name" . _String
        names `shouldBe` ["New Space"]

      it "can assert a trait" $ do
        s <- mutation "createSpace" $
               [ "name"        .= ("S" :: Text)
               , "description" .= ("" :: Text)
               ]

        let sid = s ^. v . key "spaces" . nth 0 . key "uid" . _String

        -- S |= compact
        t1 <- mutation "assertTrait" $
                [ "spaceId"     .= sid
                , "propertyId"  .= compact
                , "value"       .= True
                , "description" .= ("" :: Text)
                ]

        t1 ^.. v . key "spaces" . values . key "name" . _String `shouldBe` ["S"]

        -- > 3 traits
        -- all traits true
        -- TODO: traits include compact = true and countably compact = true
        t1 ^.. v . key "spaces" . nth 0 . key "traits" . values . key "value" . _Bool
          `shouldBe` (take 19 $ repeat True)

        -- S |= ~metrizable
        t2 <- mutation "assertTrait" $
                [ "spaceId"     .= sid
                , "propertyId"  .= metrizable
                , "value"       .= False
                , "description" .= ("" :: Text)
                ]

        t2 ^.. v . key "spaces" . values . key "name" . _String `shouldBe` ["S"]
        -- TODO: assertion about derived traits

      xit "can assert a theorem" $ do
        "this test" `shouldBe` "done"
      --   p <- mutation "createProperty" "{ properties { uid } }"
      --          [ "name"        .= ("P" :: Text)
      --          , "description" .= ("" :: Text)
      --          ]

      --   let pid = p ^. key "properties" . nth 0 . key "uid" . _String
      --       q = "{ version, spaces { name, traits { property { name } value } }, theorems { uid, if, then, description } }"

      --   -- compact => P
      --   t1 <- mutation "assertTheorem" q
      --           [ "antecedent"  .= (encodeText $ object [ compact .= True ])
      --           , "consequent"  .= (encodeText $ object [ pid .= True ])
      --           , "description" .= ("New theorem" :: Text)
      --           ]

      --   assertNotEq "version" initialVersion $
      --     t1 ^. key "version" . _String
      --   assertEq "description" ["New theorem"] $
      --     t1 ^.. key "theorems" . values . key "description" . _String

      --   -- TODO
      --   -- assert that there are spaces in the response
      --   -- for each space
      --      -- name present
      --      -- traits.count = 1
      --         -- trait property is P
      --         -- trait value is true
      --   -- also assert P => paracompact?

      --   -- P => metacompact
      --   t2 <- mutation "assertTheorem" q
      --           [ "antecedent"  .= (encodeText $ object [ pid .= True ])
      --           , "consequent"  .= (encodeText $ object [ metacompact .= True ])
      --           , "description" .= ("New theorem" :: Text)
      --           ]

      --   assertNotEq "version" initialVersion $
      --     t2 ^. key "version" . _String
      --   assertEq "description" ["New theorem"] $
      --     t2 ^.. key "theorems" . values . key "description" . _String
