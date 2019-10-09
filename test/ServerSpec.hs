{-# LANGUAGE MultiParamTypeClasses #-}
module ServerSpec (spec) where

import Test.Import

import Client
import Interpreter
import Server

-- import           Control.Concurrent        (forkIO, killThread)
-- import           Control.Monad.Fail        (MonadFail)
-- import           Data.Formula              ((.=))
-- import qualified Data.Id                   as Id
-- import           Data.Implication          ((~>))
-- import qualified Data.Property             as Property
-- import qualified Data.Space                as Space
-- import qualified Data.Theorem              as Theorem
-- import qualified Data.Trait                as Trait
-- import           Network.HTTP.Types.Status (statusCode)
-- import qualified Network.Wai.Handler.Warp  as Warp (run)
-- import qualified Persist
-- import           Servant.Client.Core       (ClientError(..), responseStatusCode) -- TODO: re-export from Client
-- import           Test.Fixtures             as Fixtures

spec :: IO TestTree
spec = testSpec "Server" $ do
  let
    eval :: Sem '[Client, Expect, Fail, Embed IO] a -> IO a
    eval action = do
      client <- initialize 31415 ""

      let
        config = Server.Config 31415 False $ Interpreter.memory testMaster testStatus

      bracket
        (liftIO $ forkIO $ start config)
        killThread
        (const $ runTest $ runClient client action)

  describe "status" $ do
    it "can fetch the status" $ eval $ do
      testStatus <== Client.status

  -- describe "branches" $ do
  --   it "has the master branch by default" $ eval $ do
  --     [testMaster] <== branches

--     it "can fetch branches" $ do
--       client $ \Client{..} -> do
--         void $ Persist.createBranch "test"

--         branches `shouldReturn`
--           [ Branch "test" Nothing
--           ]

--   describe "me" $ do
--     it "returns user information" $
--       clientFor james $ \Client{..} -> do
--         me `shouldReturn` james

--     it "404s unless authenitcated" $ do
--       client $ \Client{..} -> do
--         me `shouldFailWith` 404

--   describe "createSpace" $ do
--     let
--       input :: CreateSpaceBody
--       input = CreateSpaceBody
--           { name        = "New Space"
--           , aliases     = ["Foo", "Bar"]
--           , description = "Description"
--           , topology    = Just "Topology"
--           , refs        = []
--           }

--       expected :: SpaceId -> Space
--       expected id = Space
--         id
--         "New Space"
--         ["Foo", "Bar"]
--         "Description"
--         (Just "Topology")
--         []

--     it "can create a new space" $ clientFor james $ \Client{..} -> do
--       branch <- userBranch james

--       id <- fmap Space.id $ createSpace (branchName branch) input

--       Persist.findSpace branch id `shouldReturn` Just (expected id)

--     it "must have write access to the branch" $ clientFor leah $ \Client{..} -> do
--       branch <- userBranch james

--       createSpace (branchName branch) input `shouldFailWith` 403

--     -- TODO
--     -- it "does not allow admins to write to master" $ clientFor james $ \c@Client{..} -> do
--     --   createSpace (branchName Fixtures.master) input `shouldFailWith` 403

--   describe "updateSpace" $ do
--     it "404s if the space is not found" $ clientFor james $ \Client{..} -> do
--       branch <- branchName <$> userBranch james

--       flip shouldFailWith 404 $
--         updateSpace
--           branch
--           (Id.fromInt 999)
--           UpdateSpaceBody
--             { name        = "S999"
--             , description = "Updated"
--             , aliases     = []
--             , topology    = Nothing
--             , refs        = [counterexamplesInTopology]
--             }

--   describe "error handling" $ do
--     -- TODO:
--     -- * lensify backends
--     -- * in-memory backend w/ misconfigured github credentials should 500 and report an error
--     -- * in-memory backend w/ error call should 500 and report error
--     it "logs errors" $ client $ \_ -> do
--       () `shouldBe` ()

--   it "performs all CRUD actions" $ clientFor james $ \Client{..} -> do
--     let branchName = "users/jamesdabbs@gmail.com"

--     branch <- Persist.createBranch branchName

--     Persist.setBranchAccess james branch BranchWrite

--     sId <- fmap Space.id $ createSpace branchName $
--       CreateSpaceBody
--         { name        = "S1"
--         , description = "Initial"
--         , aliases     = []
--         , topology    = Nothing
--         , refs        = []
--         }

--     void $ updateSpace branchName sId $
--       UpdateSpaceBody
--         { name        = "S1"
--         , description = "Updated"
--         , aliases     = []
--         , topology    = Nothing
--         , refs        = [counterexamplesInTopology]
--         }

--     p1Id <- fmap Property.id $ createProperty branchName $
--       CreatePropertyBody
--         { name        = "P1"
--         , description = "Initial"
--         , aliases     = []
--         , refs        = []
--         }

--     void $ updateProperty branchName p1Id $
--       UpdatePropertyBody
--         { name        = "P1"
--         , description = "Updated"
--         , aliases     = []
--         , refs        = [counterexamplesInTopology]
--         }

--     p2Id <- fmap Property.id $ createProperty branchName $
--       CreatePropertyBody
--         { name        = "P2"
--         , description = "Initial"
--         , aliases     = ["Second"]
--         , refs        = []
--         }

--     let p1p2 = p1Id .= True ~> p2Id .= True

--     tId <- fmap Theorem.id $ createTheorem branchName $
--       CreateTheoremBody
--         { implication = p1p2
--         , converse    = Nothing
--         , description = "Initial"
--         , refs        = []
--         }

--     void $ updateTheorem branchName tId $
--       UpdateTheoremBody
--         { converse    = Nothing
--         , description = "Updated"
--         , refs        = [counterexamplesInTopology]
--         }

--     void $ createTrait branchName sId p1Id $
--       CreateTraitBody
--         { value       = Trait.Value False
--         , description = "Initial"
--         , refs        = []
--         }

--     void $ updateTrait branchName sId p1Id $
--       UpdateTraitBody
--         { description = "Updated"
--         , refs        = [counterexamplesInTopology]
--         }

--     Persist.allSpaces branch `shouldReturn`
--       [ Space
--         { id          = sId
--         , name        = "S1"
--         , description = "Updated"
--         , aliases     = []
--         , topology    = Nothing
--         , refs        = [counterexamplesInTopology]
--         }
--       ]

--     properties <- Persist.allProperties branch
--     sortOn Property.name properties `shouldBe`
--       [ Property
--         { id          = p1Id
--         , name        = "P1"
--         , description = "Updated"
--         , aliases     = []
--         , refs        = [counterexamplesInTopology]
--         }
--       , Property
--         { id          = p2Id
--         , name        = "P2"
--         , description = "Initial"
--         , aliases     = ["Second"]
--         , refs        = []
--         }
--       ]

--     Persist.allTheorems branch `shouldReturn`
--       [ Theorem
--         { id          = tId
--         , implication = p1p2
--         , converse    = Nothing
--         , description = "Updated"
--         , refs        = [counterexamplesInTopology]
--         }
--       ]

--     Persist.allTraits branch `shouldReturn`
--       [ Trait
--         { space       = sId
--         , property    = p1Id
--         , value       = Trait.Value False
--         , description = "Updated"
--         , refs        = [counterexamplesInTopology]
--         }
--       ]

--     Persist.history branch `shouldReturn`
--       map (Commit james)
--         [ "Update S1: ¬P1"
--         , "Add S1: ¬P1"
--         , "Update P1 => P2"
--         , "Add P1 => P2"
--         , "Add P2"
--         , "Update P1"
--         , "Add P1"
--         , "Update S1"
--         , "Add S1"
--         ]

-- settings :: Config
-- settings = Config 8888 (Persist.Memory testMaster)

-- withApp :: (Env -> IO ()) -> IO ()
-- withApp action = do
--   env <- boot ServerSpec.settings

--   bracket
--     (liftIO $ forkIO $ Warp.run (port ServerSpec.settings) $ Server.build env)
--     killThread
--     (const $ action env)

-- client :: (Client Server -> Server ()) -> IO ()
-- client = clientWithToken $ return ""

-- clientFor :: User -> (Client Server -> Server ()) -> IO ()
-- clientFor user = clientWithToken $ tokenUuid <$> Persist.tokenFor user

-- clientWithToken :: Server Text
--                 -> (Client Server -> Server ())
--                 -> IO ()
-- clientWithToken getToken handler = withApp $ \env -> flip runReaderT env $ unServer $
--   getToken >>= Client.build (port ServerSpec.settings) >>= handler

-- userBranch :: User -> Server Branch
-- userBranch user = do
--   branch <- Persist.createBranch $ userEmail user
--   Persist.setBranchAccess user branch BranchWrite
--   return branch

-- shouldFailWith :: (MonadIO m, MonadUnliftIO m, MonadFail m) => m a -> Int -> m ()
-- shouldFailWith action code = do
--   Left (FailureResponse _ response) <- try action
--   (statusCode $ responseStatusCode response) `shouldBe` code
