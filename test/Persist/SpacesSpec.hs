module Persist.SpacesSpec (spec) where

import Test.Import
import Test.Fixtures hiding (test)

import Persist.Spaces

import qualified Data.Space          as Space
import           Persist
import qualified Persist.Auth        as Auth
import qualified Persist.Backend.Git as Git
import qualified Persist.Branches    as Branches
import qualified Persist.Github      as Github
import qualified Persist.Repo        as Repo

spaces :: [Space]
spaces = [finiteDiscrete]

new :: Space
new = Space (CanonicalId 100) "Test" [] "" Nothing []

spec :: IO TestTree
spec = testSpec "Persist.Spaces" $ parallel $ do
  api "pure" $
    runM
      . check
      . Github.runState
      . Auth.evalState
      . Repo.ignore
      . Branches.runState testMaster
      . runState spaces

  api "io" $ \action -> withTempRepo $ \env -> do
    Git.run env $ Git.createMaster testMaster
    runM
      . check
      . Github.runState
      . Auth.evalState
      . Repo.runIO env
      . Branches.runState testMaster
      . runRepo $ do
          mapM_ (put setup) spaces
          action

api :: String
    -> (forall a. Sem '[Spaces, Branches, Repo, Auth, Github, Expect, Embed IO] a -> IO a)
    -> Spec
api label eval = do
  describe label $ do
    describe "all" $ do
      it "shows all spaces" $ eval $ do
        spaces <== all

    describe "get" $ do
      it "finds a space if it exists" $ eval $ do
        Just finiteDiscrete <== get (Space.id finiteDiscrete)

      it "does not find a space if it does not exist" $ eval $ do
        Nothing <== get (CanonicalId (-1))

    describe "put" $ do
      it "can create a space" $ eval $ do
        put setup new
        spaces <> [new] <== all
