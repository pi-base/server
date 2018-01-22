{-# LANGUAGE 
    RankNTypes
  , TypeApplications
#-}
module GraphSpec (spec) where

import Test.Tasty
import Test.Tasty.Hspec
import TestImport hiding (request)

import           Control.Lens       hiding ((.=))
import           Data.Aeson         (Value(..), (.=), object)
import           Data.Aeson.Lens
import           Data.List          (nub)
import qualified Data.Map           as M
import qualified Data.Monoid

import Graph.Common

import           Core
import qualified Data.Branch         as Branch
import qualified Graph.Queries.Cache as Cache
import qualified Graph.Root          as Root
import           Util                (encodeText)

spec :: IO (TestApp App) -> IO TestTree
spec getApp = do
  schema  <- either (throw . SchemaInvalid) return Root.schema
  queries <- Cache.mkCache schema "graph/queries"
  config  <- getApp >>= return . mkConfig >>= login testUser

  let
    run :: String -> [(Text, Value)] -> IO (Either Error Value)
    run name vars = runGraph config $ Root.asJSON (Root.compiled queries) request
      where
        request = object 
          [ "operationName" .= name
          , "variables" .= object vars
          , "query" .= ("" :: Text) -- overridden with compiled query
          ]
    
    query' :: FilePath -> [(Text, Value)] -> IO (Either Error Value)
    query' name vars = run name vars >>= \case
      Right result -> do
        let errors = result ^.. key "errors" . values . _Value
        errors `shouldBe` []
        return . Right . Object $ result ^. key "data" . _Object
      Left err -> return $ Left err

    query :: FilePath -> [(Text, Value)] -> IO Value
    query name vars = do
      result <- query' name vars
      result `shouldSatisfy` isRight
      return $ fromRight result

    -- We might want to distinguish these in the future
    mutation' :: FilePath -> [(Text, Value)] -> IO (Either Error Value)
    mutation' = query'

    mutation :: FilePath -> [(Text, Value)] -> IO Value
    mutation = query

    initial :: Sha
    initial = "b91cbfb12122fc4fc5379f7a9f68cc42c487aa81"

    resetBranch :: Text -> Sha -> IO ()
    resetBranch name sha = void $ runGraph config $ do
      Branch.reset (Branch name Nothing) (CommitSha sha)

  testSpec "Graph" $ do
    describe "query compilation" $ do
      it "compiles all queries" $ do
        errors <- Cache.loadAll queries
        errors `shouldBe` mempty

    describe "user" $ do
      it "can fetch user information" $ do
        user <- query "Me" mempty

        user ^.  key "me" . key "name" . _String `shouldBe` "test"

        let branches = buildMap (key "name" . _String) (key "access" . _String) $ user ^.. key "me" . key "branches" . values . _Value

        M.lookup "users/test" branches `shouldBe` Just "admin"
        M.lookup "master" branches `shouldBe` Just "read"

    describe "branches" $ do
      it "can reset user owned branches" $ do
        result <- mutation "ResetBranch" $
                    [ "input" .= object
                      [ "branch" .= ("users/test" :: Text)
                      , "to"     .= ("master" :: Text)
                      ]
                    ]
        result ^. key "resetBranch" . key "branch" . _String `shouldBe` "users/test"

      it "cannot reset system branches" $ do
        result <- mutation' "ResetBranch" $
              [ "input" .= object
                [ "branch" .= ("master" :: Text)
                , "to"     .= ("users/test" :: Text)
                ]
              ]
        let (Left (PermissionError (BranchPermission required))) = result
        required `shouldBe` BranchAdmin

    describe "viewer" $ do
      it "can run queries directly" $ do
        result <- query "Viewer" $ [ "version" .= initial ]

        let spaceIds = result ^.. key "viewer" . key "spaces" . values . key "uid" . _String
        length spaceIds `shouldSatisfy` (>= 143)

        let propIds = result ^.. key "viewer" . key "properties" . values . key "uid" . _String
        length propIds `shouldSatisfy` (>= 100)

        let theoremIds = result ^.. key "viewer" . key "theorems" . values . key "uid" . _String
        length theoremIds `shouldSatisfy` (>= 225)

      it "can add a space" $ do
        resetBranch "users/test" initial

        result <- mutation "CreateSpace" $
                    [ "patch" .= object
                      [ "branch" .= ("users/test" :: Text)
                      , "sha"    .= initial
                      ]
                    , "space" .= object
                      [ "uid"         .= ("s1" :: Text)
                      , "name"        .= ("New Space" :: Text)
                      , "description" .= ("Desc" :: Text)
                      ]
                    ]

        let names = result ^.. key "createSpace" . key "spaces" . values . key "name" . _String
        names `shouldBe` ["New Space"]

      it "can assert a trait" $ do
        resetBranch "users/test" initial

        s <- mutation "CreateSpace" $
               [ "patch" .= object
                 [ "branch" .= ("users/test" :: Text)
                 , "sha"    .= initial
                 ]
               , "space" .= object
                 [ "uid"         .= ("s1" :: Text)
                 , "name"        .= ("New Space" :: Text)
                 , "description" .= ("Desc" :: Text)
                 ]
               ]

        let sid = s ^. key "createSpace" . key "spaces" . nth 0 . key "uid" . _String
        sid `shouldBe` "s1"

        let v1 = s ^. key "createSpace" . key "version" . _String
        length v1 `shouldBe` 40

        -- S |= compact
        t1 <- mutation "AssertTrait" $
                [ "patch" .= object
                  [ "branch" .= ("users/test" :: Text)
                  , "sha"    .= v1
                  ]
                , "trait" .= object
                  [ "spaceId"     .= sid
                  , "propertyId"  .= compact
                  , "value"       .= True
                  , "description" .= ("" :: Text)
                  ]
                ]

        let s1 = Object $ t1 ^. key "assertTrait" . key "spaces" . nth 0 . _Object
        s1 ^. key "uid" . _String `shouldBe` sid

        let ps1 = traitMap $ s1 ^.. key "traits" . values . _Value
        M.lookup compact ps1 `shouldBe` Just True
        M.lookup paracompact ps1 `shouldBe` Just True
        -- FIXME: M.lookup metacompact ps1 `shouldBe` Just True
        M.lookup metrizable ps1 `shouldBe` Nothing

        let v2  = t1 ^. key "assertTrait" . key "version" . _String

        -- S |= ~metrizable
        t2 <- mutation "AssertTrait" $
                [ "patch" .= object
                  [ "branch" .= ("users/test" :: Text)
                  , "sha"    .= v2
                  ]
                , "trait" .= object
                  [ "spaceId"     .= sid
                  , "propertyId"  .= metrizable
                  , "value"       .= False
                  , "description" .= ("" :: Text)
                  ]
                ]

        let s2 = Object $ t2 ^. key "assertTrait" . key "spaces" . nth 0 . _Object
        let ps2 = traitMap $ s2 ^.. key "traits" . values . _Value

        M.lookup metrizable ps2 `shouldBe` Just False
        M.lookup locallyMetrizable ps2 `shouldBe` Just False

      slow "can assert a theorem" $ do
        resetBranch "users/test" initial

        p <- mutation "CreateProperty"
                [ "patch" .= object
                  [ "branch" .= ("users/test" :: Text)
                  , "sha"    .= initial
                  ]
                , "property" .= object
                  [ "uid"         .= ("p1" :: Text)
                  , "name"        .= ("P" :: Text)
                  , "description" .= ("" :: Text)
                  ]
                ]

        let pid = p ^. key "createProperty" . key "properties" . nth 0 . key "uid" . _String
            v1  = p ^. key "createProperty" . key "version" . _String

        -- compact => P
        t1 <- mutation "AssertTheorem"
                [ "patch" .= object
                  [ "branch" .= ("users/test" :: Text)
                  , "sha"    .= v1
                  ]
                , "theorem" .= object
                  [ "uid"         .= ("t1" :: Text)
                  , "antecedent"  .= (encodeText $ object [ compact .= True ])
                  , "consequent"  .= (encodeText $ object [ pid .= True ])
                  , "description" .= ("New theorem" :: Text)
                  ]
                ]

        let spaces1 = t1 ^.. key "assertTheorem" . key "spaces" . values . _Value
        length spaces1 `shouldBe` 42

        let (trait1: rest1) = nub $ t1 ^.. key "assertTheorem" . key "spaces" . values . key "traits" . values . _Value
        length rest1 `shouldBe` 0
        trait1 ^. key "property" . key "uid" . _String `shouldBe` pid
        trait1 ^? key "value" . _Bool `shouldBe` Just True

        let v2 = t1 ^. key "assertTheorem" . key "version" . _String
        v2 `shouldNotBe` v1

        -- P => metacompact
        t2 <- mutation "AssertTheorem"
                [ "patch" .= object
                  [ "branch" .= ("users/test" :: Text)
                  , "sha"    .= v2
                  ]
                , "theorem" .= object
                  [ "uid"         .= ("t2" :: Text)
                  , "antecedent"  .= (encodeText $ object [ pid .= True ])
                  , "consequent"  .= (encodeText $ object [ metacompact .= True ])
                  , "description" .= ("New theorem" :: Text)
                  ]
                ]

        let v3 = t2 ^. key "assertTheorem" . key "version" . _String
        v3 `shouldNotBe` v2

        let spaces2 = t2 ^.. key "assertTheorem" . key "spaces" . values . _Value
        length spaces2 `shouldBe` 48

        let traits2 = nub $ t2 ^.. key "assertTheorem" . key "spaces" . values . key "traits" . values . _Value
        length traits2 `shouldBe` 2 -- TODO: that is, metacompact = true & p = false

    describe "validation" $ do
      it "handles missing fields" $ do
        resetBranch "users/test" initial
        result <- mutation' "CreateSpace" $
                    [ "patch" .= object
                      [ "branch" .= ("users/test" :: Text)
                      , "sha"    .= initial
                      ]
                    , "space" .= object
                      [ "description" .= ("Desc" :: Text)
                      ]
                    ]
        let (Left (GraphError (ExecutionErrors errs))) = result
        show errs `shouldInclude` "Could not coerce Name"

      todo "handles validation errors" $ do
        resetBranch "users/test" initial
        r1 <- mutation "CreateSpace" $
              [ "patch" .= object
                [ "branch" .= ("users/test" :: Text)
                , "sha"    .= initial
                ]
              , "space" .= object
                [ "uid"         .= ("s1" :: Text)
                , "name"        .= ("New Space" :: Text)
                , "description" .= ("Desc" :: Text)
                ]
              ]
        let v1 = r1 ^. key "createSpace" . key "version" . _String

        r2 <- mutation' "CreateSpace" $
              [ "patch" .= object
                [ "branch" .= ("users/test" :: Text)
                , "sha"    .= v1
                ]
              , "space" .= object
                [ "uid"         .= ("s1" :: Text)
                , "name"        .= ("New Space" :: Text)
                , "description" .= ("Desc" :: Text)
                ]
              ]
        let (Left (ValidationError msg)) = r2
        msg `shouldBe` ValidationMessage "uid is taken"

      it "handles branch mis-matches" $ do
        resetBranch "users/test" initial
        result <- mutation' "CreateSpace" $
                    [ "patch" .= object
                      [ "branch" .= ("users/test" :: Text)
                      , "sha"    .= ("mismatch" :: Text)
                      ]
                    , "space" .= object
                      [ "uid"         .= ("s1" :: Text)
                      , "name"        .= ("New Space" :: Text)
                      , "description" .= ("Desc" :: Text)
                      ]
                    ]
        let (Left (ConflictError Conflict{..})) = result
        actualSha `shouldBe` initial
        expectedSha `shouldBe` "mismatch"

testUser :: User
testUser = User "github:1234" "test" "test@example.com" "xxx"

compact, paracompact, metacompact, metrizable, locallyMetrizable :: Text
compact           = "P000016"
paracompact       = "P000030"
metacompact       = "P000031"
metrizable        = "P000053"
locallyMetrizable = "P000082"

buildMap :: Ord k
         => Getting k Value k
         -> Getting (Data.Monoid.First v) Value v
         -> [Value]
         -> Map k v
buildMap k v = foldr f mempty
  where
    f obj acc =
      let k' = obj ^. k
          v' = obj ^? v
      in case v' of
        Nothing  -> acc
        Just v'' -> M.insert k' v'' acc

traitMap :: [Value] -> Map Text Bool
traitMap = buildMap (key "property" . key "uid" . _String) (key "value" . _Bool)
