{-# LANGUAGE TemplateHaskell #-}
module Graph.Mutations
  ( assertTheorem
  , assertTrait
  , createProperty
  , createSpace
  , resetBranch
  , updateProperty
  , updateSpace
  , updateTheorem
  , updateTrait
  ) where

import           Data.Aeson           (decode)
import qualified Data.Text.Lazy       as TL
import           Database.Persist     (Entity(..))
import           GraphQL.Resolver

import           Core
import           Data            (makeId, slugify)
import qualified Data.Branch     as Branch
import qualified Data.Git        as Git
import qualified Data.Property   as Property
import qualified Data.Space      as Space
import qualified Data.Theorem    as Theorem
import qualified Data.Trait      as Trait
import qualified Graph.Queries   as G
import qualified Graph.Schema    as G
import qualified View            as View

assertTrait :: MonadGraph m => G.PatchInput -> G.AssertTraitInput -> Handler m G.Viewer
assertTrait patch G.AssertTraitInput{..} = do
  (user, branch) <- checkPatch patch

  space    <- Space.fetch branch $ Id spaceId
  property <- Property.fetch branch $ Id propertyId
  let trait = Trait
        { _traitSpace       = space
        , _traitProperty    = property
        , _traitValue       = value
        , _traitDescription = "FIXME"
        }

      trait' = trait 
        { _traitSpace = Id spaceId
        , _traitProperty = Id propertyId
        }

      commit = CommitMeta user $ "Add " <> traitName trait
  view <- Trait.put branch commit trait'
  G.presentView view

assertTheorem :: (MonadGraph m, MonadLogger m)
              => G.PatchInput -> G.AssertTheoremInput -> Handler m G.Viewer
assertTheorem patch G.AssertTheoremInput{..} = do
  (user, branch) <- checkPatch patch

  a <- parseFormula antecedent
  c <- parseFormula consequent

  let theorem = Theorem
        { theoremId = Id uid
        , theoremImplication = (Implication a c)
        , theoremConverse = Nothing
        , theoremDescription = "FIXME"
        }
  theorem' <- mapM (Property.fetch branch) theorem
  let meta = CommitMeta user $ "Add " <> theoremName theorem'
  view <- Theorem.put branch meta theorem
  G.presentView view

createSpace :: MonadGraph m => G.PatchInput -> G.CreateSpaceInput -> Handler m G.Viewer
createSpace patch G.CreateSpaceInput{..} = do
  (user, branch) <- checkPatch patch

  let space = Space
        { spaceId          = Id uid
        , spaceName        = name
        , spaceAliases     = []
        , spaceDescription = description
        , spaceSlug        = slugify name
        , spaceTopology    = Nothing
        }
      commit = CommitMeta user $ "Add " <> name

  (s, sha) <- Space.put branch commit space
  G.presentView $ View.build [s] [] [] [] $ Version sha

createProperty :: MonadGraph m => G.PatchInput -> G.CreatePropertyInput -> Handler m G.Viewer
createProperty patch G.CreatePropertyInput{..} = do
  (user, branch) <- checkPatch patch

  let property = Property
        { propertyId          = Id uid
        , propertyName        = name
        , propertyDescription = description
        , propertySlug        = slugify name
        , propertyAliases     = []
        }
      commit = CommitMeta user $ "Add " <> name
  (p, sha) <- Property.put branch commit property
  G.presentView $ View.build [] [p] [] [] $ Version sha

resetBranch :: MonadGraph m => G.ResetBranchInput -> Handler m G.ResetBranchResponse
resetBranch G.ResetBranchInput{..} = do
  user <- requireUser
  Branch.find branch >>= \case
    Nothing -> throw $ NotFound $ NotFoundError "Branch" branch
    Just branch' -> do
      access <- Branch.access user branch'
      unless (access == Just BranchAdmin) $ do
        throw $ PermissionError $ BranchPermission BranchAdmin
      -- TODO: handle case where `to` is not found
      commit <- Git.commitFromLabel $ Just to
      sha <- Branch.reset branch' $ CommitSha $ Git.commitSha commit

      return $ pure "ResetBranchResponse"
        :<> pure branch
        :<> pure sha

updateProperty :: MonadGraph m => G.PatchInput -> G.UpdatePropertyInput -> Handler m G.Viewer
updateProperty patch G.UpdatePropertyInput{..} = do
  (user, branch) <- checkPatch patch

  old <- Property.fetch branch $ Id uid
  let updated = old { propertyDescription = description }
      commit  = CommitMeta user $ "Update " <> propertyName updated
  (p, sha) <- Property.put branch commit updated
  G.presentView $ View.build [] [p] [] [] $ Version sha

updateSpace :: MonadGraph m => G.PatchInput -> G.UpdateSpaceInput -> Handler m G.Viewer
updateSpace patch G.UpdateSpaceInput{..} = do
  (user, branch) <- checkPatch patch

  old <- Space.fetch branch $ Id uid
  let updated = old { spaceDescription = description }
      meta    = CommitMeta user $ "Update " <> spaceName updated
  (s, sha) <- Space.put branch meta updated
  G.presentView $ View.build [s] [] [] [] $ Version sha

updateTheorem :: (MonadGraph m, MonadLogger m)
              => G.PatchInput -> G.UpdateTheoremInput -> Handler m G.Viewer
updateTheorem patch G.UpdateTheoremInput{..} = do
  (user, branch) <- checkPatch patch

  old <- Theorem.fetch branch $ Id uid
  let updated = old { theoremDescription = description }
      meta    = CommitMeta user $ "Update " <> theoremName updated
  Theorem.put branch meta (propertyId <$> updated) >>= G.presentView

updateTrait :: (MonadGraph m, MonadLogger m)
            => G.PatchInput -> G.UpdateTraitInput -> Handler m G.Viewer
updateTrait patch G.UpdateTraitInput{..} = do
  (user, branch) <- checkPatch patch

  old <- Trait.fetch branch (Id spaceId) (Id propertyId)
  let meta = CommitMeta user $ "Update " <> traitName old
      updated = old 
        { _traitDescription = description 
        , _traitSpace       = Id spaceId
        , _traitProperty    = Id propertyId
        }
  Trait.put branch meta updated >>= G.presentView

-- Helpers

-- TODO: should this return a loader for the branch?
checkPatch :: MonadGraph m => G.PatchInput -> m (User, Branch)
checkPatch G.PatchInput{..} = do
  user <- requireUser
  mb   <- Branch.find branch
  case mb of
    Nothing -> throw $ NotFound $ NotFoundError "Branch" branch
    Just b -> do
      access <- Branch.access user b
      unless (access == Just BranchAdmin || access == Just BranchWrite) $
        throw $ PermissionError $ BranchPermission BranchWrite

      currentSha <- Branch.headSha b
      unless (sha == currentSha) $
        throw $ ConflictError $ Conflict currentSha sha

      return $ (entityVal user, b)

parseFormula :: MonadGraph m => Text -> m (Formula PropertyId)
parseFormula text = case decode $ encodeUtf8 $ TL.fromStrict text of
  Nothing -> throw $ ParseError "formula" (show text)
  Just f  -> return $ Id <$> f