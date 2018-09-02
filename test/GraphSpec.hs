{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
module GraphSpec (spec) where

import Test.Tasty
import Test.Tasty.Hspec hiding (shouldBe)
import TestImport       hiding (first, head, json, request, shouldBe, update)

import           Control.Lens       hiding ((.=))
import           Data.Aeson         (Value(..), (.=), object)
import           Data.Aeson.Lens    hiding (key)
import qualified Data.Aeson.Lens    as L (key)
import qualified Data.Map           as M
import qualified Data.Monoid

import Graph.Common

import           Core
import qualified Data.Branch         as Branch
import           Util                (encodeText)

initial :: Sha
initial = "d71e74370ea1d293197fdffd5f89c357ed45a273"

reset :: G ()
reset = void $ do
  eu <- login testUser
  Branch.reset (Branch.forUser eu) $ CommitSha initial

setup :: G ()
setup = Branch.ensureBaseBranch >> Branch.claimUserBranches

spec :: IO (TestApp App) -> IO TestTree
spec getApp = do
  graph <- mkGraph getApp

  let
    g :: forall a. G a -> IO a
    g = runG graph

  -- TODO: we only _really_ need to reset after mutations
  testSpec "Graph" $ beforeAll (g setup) $ before_ (g reset) $ do
    describe "Me" $ do
      it "can fetch user data" $ do
        user <- g $ run "Me" noVariables

        user^.k "me".name `shouldBe` "test"

        let branches = buildMap name (k "access"._String) $ user ^.. k "me".k "branches".values._Value

        M.lookup testBranch branches `shouldBe` Just "admin"
        M.lookup "master" branches `shouldBe` Just "read"

    describe "Introspection" $ do
      it "can introspect defined types" $ do
        result <- g $ run "Introspection" noVariables
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
        result <- g $ run "ResetBranch" [json|{
          "input": {
            "branch": #{testBranch},
            "to":     "master"
          }
        }|]
        result^.k "resetBranch".version `shouldBe` head

      it "cannot reset system branches" $ do
        Left BranchPermissionRequired{..} <- try . g $ run "ResetBranch" [json|{
          "input": {
            "branch": "master",
            "to": #{testBranch}
          }
        }|]
        branchName branch `shouldBe` "master"
        required `shouldBe` BranchAdmin
        actual `shouldBe` Just BranchRead

    describe "viewer" $ do
      it "can run queries directly" $ do
        result <- g $ run "Viewer" [json|{ "version": #{initial} }|]

        let spaceIds = result ^.. viewer.spaces.values.uid
        length spaceIds `shouldSatisfy` (>= 135)

        let propIds = result ^.. viewer.properties.values.uid
        length propIds `shouldSatisfy` (>= 100)

        let theoremIds = result ^.. viewer.theorems.values.uid
        length theoremIds `shouldSatisfy` (>= 200)

      it "can add a space" $ do
        result <- g $ run "CreateSpace" [json|{
          "patch": {
            "branch": #{testBranch},
            "sha":    #{initial}
          },
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
      it "can assert a trait" $ g $ do
        void checkoutUserBranch

        s <- update "CreateSpace" [json|{
          "space": {
            "name": "New Space"
          }
        }|]

        let n   = s ^. createSpace.spaces.first.name
            sid = s ^. createSpace.spaces.first.uid

        n `shouldBe` "New Space"

        -- S |= compact
        t1 <- update "AssertTrait" [json|{
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
        t2 <- update "AssertTrait" [json|{
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

      it "can assert a theorem" $ g $ do
        void checkoutUserBranch

        p <- update "CreateProperty" [json|{
          "property": {
            "name": "P"
          }
        }|]

        let pid = p ^. createProperty.properties.first.uid

        -- compact => P
        t1 <- update "AssertTheorem" [json|{
          "theorem": {
            "antecedent":  #{encodeText $ object [ compact .= True ]},
            "consequent":  #{encodeText $ object [ pid .= True ]},
            "description": "T"
          }
        }|]

        length (t1 ^.. assertTheorem.theorems.values._Value) `shouldBe` 1

        -- P => metacompact
        t2 <- update "AssertTheorem" [json|{
          "theorem": {
            "antecedent":  #{encodeText $ object [ pid .= True ]},
            "consequent":  #{encodeText $ object [ metacompact .= True ]},
            "description": "U"
          }
        }|]

        length (t2 ^.. assertTheorem.theorems.values._Value) `shouldBe` 1

    describe "validation" $ do
      it "handles missing fields" $ do
        Left (ExecutionErrors errs) <- try . g $ run "CreateSpace" [json|{
          "patch": {
            "branch": #{testBranch},
            "sha":    #{initial}
          },
          "space": {
            "description": ""
          }
        }|]
        show errs `shouldInclude` "Could not coerce Name"

      it "handles branch mis-matches" $ do
        Left ConflictError{..} <- try . g $ run "CreateSpace" [json|{
          "patch": {
            "branch": #{testBranch},
            "sha":    "mismatch"
          },
          "space": {
            "name": "New Space"
          }
        }|]
        actualSha   `shouldBe` initial
        expectedSha `shouldBe` "mismatch"

    describe "Branch approval" $ do
      ci "works end-to-end" $ g $ do
        -- User creates a branch
        _ <- login cody
        eb@(Entity _ branch) <- checkoutUserBranch

        _ <- update "ResetBranch" [json|{
          "input": {
            "branch": #{branchName branch},
            "to":     #{head}
          }
        }|]

        v <- update "CreateProperty" [json|{
          "property": {
            "name": "P"
          }
        }|]
        let pid = v ^. createProperty.properties.first.uid

        _ <- update "AssertTheorem" [json|{
          "theorem": {
            "antecedent":  #{encodeText $ object [ pid .= True ]},
            "consequent":  #{encodeText $ object [ compact .= True ]},
            "description": "T"
          }
        }|]

        _ <- update "AssertTrait" [json|{
          "trait": {
            "spaceId":    "S000001",
            "propertyId": #{pid},
            "value":      true
          }
        }|]

        -- Admin approves the branch
        sxc    <- login steven
        master <- Branch.ensureBaseBranch
        Branch.grant sxc master BranchAdmin
        Branch.grant sxc eb     BranchAdmin

        checkout branch
        v' <- update "ApproveBranch" [json|{
          "input": {
            "branch": #{branchName branch}
          }
        }|]

        length (v' ^. approveBranch.version) `shouldBe` 40

testUser :: User
testUser = User "github:1234" "test" "test@example.com" "xxx" False

testBranch :: Text
testBranch = "users/" <> userEmail testUser

steven, cody :: User
steven = userNamed "steven"
cody   = userNamed "cody"

userNamed :: Text -> User
userNamed n = User ("github:" <> n) n (n <> "@example.com") n False

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
