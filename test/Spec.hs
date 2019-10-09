module Main
  ( main
  ) where

import Test.Import

import Test.Tasty (defaultMain)

import qualified Data.FormulaSpec
import qualified Data.IdSpec
import qualified Data.MemoSpec
import qualified Data.SpaceSpec
import qualified Persist.Backend.Git.PageSpec
import qualified Persist.Backend.Git.PagesSpec
import qualified Persist.BranchesSpec
import qualified Persist.DBSpec
import qualified Persist.GithubSpec
import qualified Persist.HttpSpec
import qualified Persist.SpacesSpec
import qualified Server.ApiSpec
import qualified ServerSpec

-- TODO:
-- * Working CLI for branch and user management
-- * Auth for local dev
-- * Functional OAuth with github
-- * Test coverage
-- * Rollbar error handling
-- * Outside-in tests using servant-client
-- * Branch merging
-- * Cleanup TODOs, `error` calls, `hiding`s
-- * End-to-end test against production (minus http) interpreter
-- * Verify Reason codegen
main :: IO ()
main = do
  dbEnv <- setupDB

  specs <- sequence
    [ Data.FormulaSpec.spec
    -- , Data.IdSpec.spec
    -- , Data.MemoSpec.spec
    -- , Data.SpaceSpec.spec
    -- , Persist.Backend.Git.PageSpec.spec
    -- , Persist.Backend.Git.PagesSpec.spec
    -- , Persist.BranchesSpec.spec dbEnv
    , Persist.DBSpec.spec dbEnv
    -- , Persist.GithubSpec.spec
    -- , Persist.HttpSpec.spec
    -- , Persist.SpacesSpec.spec
    -- , Server.ApiSpec.spec
    -- , ServerSpec.spec
    ]

  defaultMain $ testGroup "Pi-Base" specs
