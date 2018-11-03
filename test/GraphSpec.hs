{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
module GraphSpec (spec) where

import           Test.Import  hiding (first, property)
import qualified Graph.Client as Client

import           Control.Lens       hiding ((.=))
import           Data.Aeson         (Value(..), (.=), object)
import           Data.Aeson.Lens    hiding (key)
import qualified Data.Aeson.Lens    as L (key)
import qualified Data.Map           as M
import qualified Data.Monoid        (First)
import qualified Data.Text          as T

import qualified Data.Branch  as Branch
import           Graph.Types  (Name)
import           Util         (encodeText, findOrCreate)

reset :: TestM ()
reset = do
  eu <- db $ findOrCreate (UniqueUser . userEmail) testUser
  (Entity _ branch) <- Branch.ensureUserBranch eu
  Branch.reset_ branch $ CommitSha headSha

spec :: Runner -> TestM TestTree
spec run = do
  client <- Client.initialize

  let
    g :: Name -> Value -> IO Value
    g query body = run $ Client.run client query body

    as :: User -> IO ()
    as user = void $ run $ Client.login client user

    editing :: IO ()
    editing = void $ run $ Client.checkoutUserBranch client

  -- TODO: we only _really_ need to reset after mutations
  specify "Graph" $ before_ (run GraphSpec.reset) $ do
    describe "Me" $ do
      it "can fetch user data" $ do
        as testUser

        user <- g "Me" noVariables

        user^.k "me".name `shouldBe` "test"

        let branches = buildMap name (k "access"._String) $ user ^.. k "me".k "branches".values._Value

        M.lookup "users/test@example.com" branches `shouldBe` Just "admin"
        M.lookup "test" branches `shouldBe` Just "read"

    describe "Introspection" $ do
      it "can introspect defined types" $ do
        result <- g "Introspection" noVariables
        let types = result ^..k "__schema".k "types".values.name
        types `shouldBe` [ "AssertTheoremInput"
                         , "AssertTraitInput"
                         , "Branch"
                         , "BranchInput"
                         , "Citation"
                         , "CitationInput"
                         , "CreatePropertyInput"
                         , "CreateSpaceInput"
                         , "Mutation"
                         , "PatchInput"
                         , "Property"
                         , "Query"
                         , "ResetBranchInput"
                         , "Space"
                         , "SubmitBranchResponse"
                         , "Theorem"
                         , "Trait"
                         , "UpdatePropertyInput"
                         , "UpdateSpaceInput"
                         , "UpdateTheoremInput"
                         , "UpdateTraitInput"
                         , "User"
                         , "Viewer"
                         ]

    describe "branches" $ do
      it "can reset user owned branches" $ do
        as testUser

        result <- g "ResetBranch" [json|{
          "input": {
            "branch": "users/test@example.com",
            "to":     #{headSha}
          }
        }|]
        result^.k "resetBranch".version `shouldBe` headSha

      it "cannot reset system branches" $ do
        as testUser

        Left BranchPermissionRequired{..} <- try $ g "ResetBranch" [json|{
          "input": {
            "branch": "master",
            "to": #{headSha}
          }
        }|]

        branchName branch `shouldBe` "master"
        required `shouldBe` BranchAdmin
        actual `shouldBe` Just BranchRead

    describe "Viewer" $ do
      it "can run queries directly" $ do
        result <- g "Viewer" [json|{ "version": #{headSha} }|]

        let spaceIds = result ^.. viewer.spaces.values.uid
        length spaceIds `shouldSatisfy` (>= 135)

        let propIds = result ^.. viewer.properties.values.uid
        length propIds `shouldSatisfy` (>= 100)

        let theoremIds = result ^.. viewer.theorems.values.uid
        length theoremIds `shouldSatisfy` (>= 180)

      it "can add a space" $ do
        as testUser
        editing

        result <- g "CreateSpace" [json|{
          "space": {
            "uid":  "s1",
            "name": "New Space",
            "references": [
              {
                "name": "Ref",
                "type": "wikipedia",
                "ref":  "wiki://ref"
              }
            ]
          }
        }|]

        let names = result ^.. createSpace.spaces.values.name
        names `shouldBe` ["New Space"]

      -- TODO: refactor `run` => `update` and skip patch tracking
      it "can assert a trait" $ do
        as testUser
        editing

        s <- g "CreateSpace" [json|{
          "space": {
            "name": "New Space"
          }
        }|]

        let n   = s ^. createSpace.spaces.first.name
            sid = s ^. createSpace.spaces.first.uid

        n `shouldBe` "New Space"

        -- S |= compact
        t1 <- g "AssertTrait" [json|{
          "trait": {
            "spaceId":    #{sid},
            "propertyId": #{compact},
            "value":      true
          }
        }|]

        let s1 = Object $ t1 ^. assertTrait.spaces.first._Object
        s1 ^. uid `shouldBe` sid

        let ps1 = traitMap $ s1 ^.. traits.values._Value
        M.lookup compact ps1 `shouldBe` Just True
        -- M.lookup paracompact ps1 `shouldBe` Just True
        -- M.lookup metrizable ps1 `shouldBe` Just True

        -- S |= ~metrizable
        t2 <- g "AssertTrait" [json|{
          "trait": {
            "spaceId":    #{sid},
            "propertyId": #{metrizable},
            "value":      false
          }
        }|]

        let s2 = Object $ t2 ^. assertTrait.spaces.first._Object
        let ps2 = traitMap $ s2 ^.. traits.values._Value

        M.lookup metrizable ps2 `shouldBe` Just False
        -- M.lookup locallyMetrizable ps2 `shouldBe` Just False

      it "can assert a theorem" $ do
        as testUser
        editing

        p <- g "CreateProperty" [json|{
          "property": {
            "name": "P"
          }
        }|]

        let pid = p ^. createProperty.properties.first.uid

        -- compact => P
        t1 <- g "AssertTheorem" [json|{
          "theorem": {
            "antecedent":  #{encodeText $ object [ compact .= True ]},
            "consequent":  #{encodeText $ object [ pid .= True ]},
            "description": "T"
          }
        }|]

        length (t1 ^.. assertTheorem.theorems.values._Value) `shouldBe` 1

        -- P => metacompact
        t2 <- g "AssertTheorem" [json|{
          "theorem": {
            "antecedent":  #{encodeText $ object [ pid .= True ]},
            "consequent":  #{encodeText $ object [ metacompact .= True ]},
            "description": "U"
          }
        }|]

        length (t2 ^.. assertTheorem.theorems.values._Value) `shouldBe` 1

    describe "validation" $ do
      todo "handles missing fields" $ do
        as testUser
        editing

        e <- g "CreateSpace" [json|{
          "space": {}
        }|]
        traceS e
        -- show errs `shouldInclude` "Could not coerce Name"

      it "handles branch mis-matches" $ do
        as testUser
        editing

        let oldSha = "3885a2d01d34033ee2dc55048e69f5c586522c3c"

        Left ConflictError{..} <- try $ g "CreateSpace" [json|{
          "patch": {
            "branch": "users/test@example.com",
            "sha":    #{oldSha}
          },
          "space": {
            "name": "New Space"
          }
        }|]
        actualSha   `shouldBe` headSha
        expectedSha `shouldBe` oldSha

    describe "Branch approval" $ do
      ci "works end-to-end" $ do
        -- User creates a branch
        as cody
        editing

        _ <- g "ResetBranch" [json|{
          "input": {
            "branch": "users/cody@pi-base.org",
            "to":     #{headSha}
          }
        }|]

        v <- g "CreateProperty" [json|{
          "property": {
            "name": "P"
          }
        }|]
        let pid = v ^. createProperty.properties.first.uid

        _ <- g "AssertTheorem" [json|{
          "theorem": {
            "antecedent":  #{encodeText $ object [ pid .= True ]},
            "consequent":  #{encodeText $ object [ compact .= True ]},
            "description": "T"
          }
        }|]

        _ <- g "AssertTrait" [json|{
          "trait": {
            "spaceId":    "S000001",
            "propertyId": #{pid},
            "value":      true
          }
        }|]

        -- Admin approves the branch
        as steven
        v' <- g "ApproveBranch" [json|{
          "input": {
            "branch": "users/cody@pi-base.org"
          }
        }|]

        T.length (v' ^. approveBranch.version) `shouldBe` 40

compact, metacompact, metrizable :: Text
compact           = "P000016"
metacompact       = "P000031"
metrizable        = "P000053"
-- paracompact       = "P000030"
-- locallyMetrizable = "P000082"

buildMap :: Ord k
         => Getting k Value k
         -> Getting (Data.Monoid.First v) Value v
         -> [Value]
         -> Map k v
buildMap key val = foldr f mempty
  where
    f obj acc =
      let k' = obj ^. key
          v' = obj ^? val
      in case v' of
        Nothing  -> acc
        Just v'' -> M.insert k' v'' acc

traitMap :: [Value] -> Map Text Bool
traitMap = buildMap (property . uid) (k "value" . _Bool)

-- Convenience names

k :: Text -> Traversal' Value Value
k = L.key

noVariables :: Value
noVariables = [json|{}|]

first :: Traversal' Value Value
first = nth 0

viewer, spaces, properties, property, theorems, traits :: Traversal' Value Value
viewer      = k "viewer"
spaces      = k "spaces"
properties  = k "properties"
property    = k "property"
theorems    = k "theorems"
traits      = k "traits"

approveBranch, assertTheorem, assertTrait, createProperty, createSpace :: Traversal' Value Value
approveBranch  = k "approveBranch"
assertTheorem  = k "assertTheorem"
assertTrait    = k "assertTrait"
createSpace    = k "createSpace"
createProperty = k "createProperty"

name, uid, version :: Traversal' Value Text
name    = k "name"    . _String
uid     = k "uid"     . _String
version = k "version" . _String
