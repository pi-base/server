{-# LANGUAGE TemplateHaskell #-}
module Graph.Mutations
  ( assertTheorem
  , assertTrait
  , createProperty
  , createSpace
  , resetBranch
  , testReset
  , updateProperty
  , updateSpace
  , updateTheorem
  ) where

import           Control.Monad.Logger (logInfo)
import           Data.Aeson           (decode)
import qualified Data.Text.Lazy       as TL
import           Database.Persist     (Entity(..))
import           GraphQL.Resolver

import           Core
import           Data            (makeId, slugify)
import qualified Data.Branch     as Branch
import qualified Data.Property   as Property
import qualified Data.Space      as Space
import qualified Data.Theorem    as Theorem
import qualified Data.Trait      as Trait
import qualified Graph.Types     as G
import qualified Graph.Queries   as G
import           Handler.Helpers (ensureUser, ensureToken)
import           Settings        (appTestMode)
import qualified View            as View

assertTrait :: MonadGraph m => G.AssertTraitInput -> Handler m G.Viewer
assertTrait G.AssertTraitInput{..} = do
  (Entity _ user) <- requireUser

  let trait = Trait
        { _traitSpace       = Id spaceId
        , _traitProperty    = Id propertyId
        , _traitValue       = value
        , _traitDescription = "FIXME"
        }

      commit = CommitMeta user $ "Add " <> tshow trait

  view <- Trait.put (userBranch user) commit trait
  either throw G.presentView view

assertTheorem :: MonadGraph m => G.AssertTheoremInput -> Handler m G.Viewer
assertTheorem G.AssertTheoremInput{..} = do
  (Entity _ user) <- requireUser

  a <- parseFormula antecedent
  c <- parseFormula consequent

  theorem <- Theorem
    <$> makeId "t"
    <*> pure (Implication a c)
    <*> pure Nothing
    <*> pure description

  let commit = CommitMeta user $ "Add " <> tshow theorem

  Theorem.put (userBranch user) commit theorem >>= either throw G.presentView

createSpace :: MonadGraph m => G.CreateSpaceInput -> Handler m G.Viewer
createSpace G.CreateSpaceInput{..} = do
  (Entity _ user) <- requireUser

  let space = Space
        { spaceId          = Space.pending
        , spaceName        = name
        , spaceAliases     = []
        , spaceDescription = description
        , spaceSlug        = slugify name
        , spaceTopology    = Nothing
        }
      commit = CommitMeta user $ "Add " <> name

  (version, s) <- Space.put (userBranch user) commit space
  G.presentView $ View.build [s] [] [] [] version

createProperty :: MonadGraph m => G.CreatePropertyInput -> Handler m G.Viewer
createProperty G.CreatePropertyInput{..} = do
  (Entity _id user) <- requireUser

  let property = Property
        { propertyId          = Property.pending
        , propertyName        = name
        , propertyDescription = description
        , propertySlug        = slugify name
        , propertyAliases     = []
        }
      commit = CommitMeta user $ "Add " <> name
  (version, p) <- Property.put (userBranch user) commit property
  G.presentView $ View.build [] [p] [] [] version

resetBranch :: MonadGraph m => G.ResetBranchInput -> Handler m G.ResetBranchResponse
resetBranch G.ResetBranchInput{..} = do
  user <- requireUser
  Branch.find branch >>= \case
    Nothing -> throw $ NotFound branch
    Just branch' -> do
      access <- Branch.access user branch'
      unless (access == BranchAdmin || access == BranchWrite) $ do
        throw $ PermissionError "Not allowed access on branch"
      sha <- Branch.reset branch' $ CommitSha to

      return $ pure "ResetBranchResponse"
        :<> pure branch
        :<> pure sha

testReset :: MonadGraph m => G.TestResetInput -> Handler m G.TestResetResponse
testReset G.TestResetInput{..} = do
  testing <- appTestMode <$> getSettings
  if testing
    then $(logInfo) $ "Resetting test branch to " <> ref
    else error "Set TEST_MODE to allow resetting data"

  user@(Entity _id _) <- ensureUser testUser
  branch              <- Branch.ensureUserBranch user
  sha                 <- Branch.reset branch $ CommitRef $ Ref ref

  $(logInfo) $ "Reset " <> (tshow branch) <> " to " <> sha

  maybe (return ()) (void . ensureToken _id) token

  return $ pure "TestResetResponse"
    :<> pure sha
    :<> pure token

updateProperty :: MonadGraph m => G.UpdatePropertyInput -> Handler m G.Viewer
updateProperty G.UpdatePropertyInput{..} = do
  (Entity _id user) <- requireUser

  let ref = userBranch user
  old <- Property.fetch (CommitRef ref) $ Id uid
  let updated = old { propertyDescription = description }
      commit  = CommitMeta user $ "Update " <> propertyName updated
  (version, p) <- Property.put ref commit updated
  G.presentView $ View.build [] [p] [] [] version

updateSpace :: MonadGraph m => G.UpdateSpaceInput -> Handler m G.Viewer
updateSpace G.UpdateSpaceInput{..} = do
  (Entity _id user) <- requireUser

  let ref = userBranch user
  old <- Space.fetch (CommitRef ref) $ Id uid
  let updated = old { spaceDescription = description }
      commit  = CommitMeta user $ "Update " <> spaceName updated
  (version, s) <- Space.put ref commit updated
  G.presentView $ View.build [s] [] [] [] version

updateTheorem :: MonadGraph m => G.UpdateTheoremInput -> Handler m G.Viewer
updateTheorem G.UpdateTheoremInput{..} = do
  error "updateTheorem"
  -- (Entity _id user) <- requireToken
  -- let ref = userBranch user
  -- old <- T.fetch (CommitRef ref) $ TheoremId uid
  -- let updated = old { theoremDescription = description }
  --     commit  = CommitMeta user $ "Update " <> theoremName updated
  -- (version, t) <- T.put ref commit updated
  -- G.viewR $ V.build [] [] [] [t] version

-- Helpers

-- FIXME: this is defined too many places ...
userBranch :: User -> Ref
userBranch u = Ref $ "users/" <> userIdent u

parseFormula :: MonadGraph m => Text -> m (Formula PropertyId)
parseFormula text = case decode $ encodeUtf8 $ TL.fromStrict text of
  Nothing -> throw $ ParseError "formula" (show text)
  Just f  -> return $ Id <$> f

testUser :: User
testUser = User "tester" "tester" "test@pi-base.org" "xxx"
