{-# LANGUAGE DeriveGeneric #-}
module Graph.Mutations.TestReset
  ( TestResetInput
  , testReset
  ) where

import Graph.Import

import           Core
import           Data.Git
import qualified Graph.Types as G

data TestResetInput = TestResetInput { token :: Maybe Text, ref :: Text }
  deriving (Show, Generic)

instance FromValue TestResetInput
instance HasAnnotatedInputType TestResetInput
instance Defaultable TestResetInput where
  defaultFor _ = error "No default for TestResetInput"

testUser :: User
testUser = User "tester" "tester" "test@pi-base.org" "xxx"

testReset :: TestResetInput -> G G.TestResetResponse
testReset TestResetInput{..} = do
  testing <- appTestMode . appSettings <$> getYesod
  if testing
    then $(logInfo) $ "Resetting test branch to " <> ref
    else error "Set TEST_MODE to allow resetting data"

  Entity _id user <- ensureUser testUser
  branch          <- ensureUserBranch user
  version         <- resetRef branch $ CommitRef $ Ref ref

  $(logInfo) $ "Reset " <> (tshow branch) <> " to " <> (unVersion version)

  maybe (return ()) (void . ensureToken _id) token

  return $ pure "TestResetResponse"
    :<> pure (unVersion version)
    :<> pure token
