module Main
  ( main
  ) where

import Test.Import
import Test.Tasty (defaultMain)

import qualified Config      (test)
import qualified Config.Boot as Config (boot)
import qualified Data.Branch as Branch

import qualified Data.ParseSpec
import qualified Data.SpaceSpec
import qualified GraphSpec
import qualified PageSpec
import qualified Server.ApiSpec
import qualified Server.Api.AuthSpec
import qualified Server.Api.GraphSpec
import qualified Services.GithubSpec

main :: IO ()
main = do
  env <- Config.boot =<< Config.test

  testEnv <- TestEnv
    <$> pure env
    <*> newIORef Nothing
    <*> newIORef []
    <*> newIORef []

  let
    run :: Runner
    run = runTest testEnv

  run $ do
    void $ Branch.ensureBaseBranch
    void $ Branch.claimUserBranches

  tests <- specs run

  defaultMain $ testGroup "Pi-Base" tests

specs :: Runner -> IO [TestTree]
specs run = run $ sequence
  [ Data.ParseSpec.spec run
  , Data.SpaceSpec.spec run
  , GraphSpec.spec run
  , PageSpec.spec
  , Server.ApiSpec.spec run
  , Server.Api.AuthSpec.spec run
  , Server.Api.GraphSpec.spec run
  , Services.GithubSpec.spec run
  ]
