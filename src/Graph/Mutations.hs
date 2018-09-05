{-# LANGUAGE TemplateHaskell #-}
module Graph.Mutations
  ( mutations
  , assertTheorem
  , assertTrait
  , createProperty
  , createSpace
  , updateProperty
  , updateSpace
  , updateTheorem
  , updateTrait
  -- Branch tools
  , resetBranch
  , submitBranch
  , approveBranch
  ) where

import Protolude hiding (throwIO, to)

import           Import           (GithubSettings)
import           Database.Persist (Entity(..))
import           GraphQL.Resolver

import           Core
import           Data.Store        (storeBaseBranch)
import qualified Data.Storable     as Store
import qualified Data.Branch       as Branch
import qualified Data.Branch.Merge as Branch
import qualified Data.Git          as Git
import qualified Data.Id           as Id
import qualified Data.Theorem      as Theorem
import qualified Data.Trait        as Trait
import qualified Graph.Queries     as G
import qualified Graph.Schema      as G
import qualified Services.Github   as Github
import qualified View              as View

mutations :: MonadGraph m => GithubSettings -> Handler m G.MutationRoot
mutations settings =
  pure $ createSpace
     :<> createProperty
     :<> updateSpace
     :<> updateProperty
     :<> updateTheorem
     :<> updateTrait
     :<> assertTrait
     :<> assertTheorem
     :<> resetBranch
     :<> submitBranch settings
     :<> approveBranch

assertTrait :: MonadGraph m => G.PatchInput -> G.AssertTraitInput -> Handler m G.Viewer
assertTrait patch G.AssertTraitInput{..} = do
  (user, branch) <- checkPatch patch

  space    <- Store.fetch branch spaceId
  property <- Store.fetch branch propertyId
  let trait = Trait
        { _traitSpace       = space
        , _traitProperty    = property
        , _traitValue       = value
        , _traitRefs        = citations $ fromMaybe [] references
        , _traitDescription = fromMaybe "" description
        }

      trait' = trait
        { _traitSpace = spaceId
        , _traitProperty = propertyId
        }

      commit = CommitMeta user $ "Add " <> traitName trait
  view <- Trait.put branch commit trait'
  G.presentView view

assertTheorem :: (MonadGraph m, MonadLogger m)
              => G.PatchInput -> G.AssertTheoremInput -> Handler m G.Viewer
assertTheorem patch G.AssertTheoremInput{..} = do
  (user, branch) <- checkPatch patch

  theorem <- Id.assign $ Theorem
        { theoremId          = Id.pending
        , theoremImplication = (Implication antecedent consequent)
        , theoremConverse    = Nothing -- TODO
        , theoremRefs        = citations $ fromMaybe [] references
        , theoremDescription = fromMaybe "" description
        }
  theorem' <- mapM (Store.fetch branch) theorem
  (t, sha) <- Theorem.put branch user ("Add " <> theoremName theorem') (fmap propertyId theorem')
  G.presentView $ View.build [] [] [] [t] $ Just $ Version sha

createSpace :: MonadGraph m => G.PatchInput -> G.CreateSpaceInput -> Handler m G.Viewer
createSpace patch G.CreateSpaceInput{..} = do
  (user, branch) <- checkPatch patch

  space <- Id.assign $ Space
        { spaceId          = Id.pending
        , spaceCx          = Nothing
        , spaceName        = name
        , spaceAliases     = []
        , spaceRefs        = citations $ fromMaybe [] references
        , spaceTopology    = Nothing
        , spaceDescription = fromMaybe "" description
        }

  (s, sha) <- Store.put branch user ("Add " <> name) space
  G.presentView $ View.build [s] [] [] [] $ Just $ Version sha

createProperty :: MonadGraph m => G.PatchInput -> G.CreatePropertyInput -> Handler m G.Viewer
createProperty patch G.CreatePropertyInput{..} = do
  (user, branch) <- checkPatch patch

  property <- Id.assign $ Property
        { propertyId          = Id.pending
        , propertyCx          = Nothing
        , propertyName        = name
        , propertyAliases     = []
        , propertyRefs        = citations $ fromMaybe [] references
        , propertyDescription = fromMaybe "" description
        }
  (p, sha) <- Store.put branch user ("Add " <> name) property
  G.presentView $ View.build [] [p] [] [] $ Just $ Version sha

resetBranch :: MonadGraph m => G.ResetBranchInput -> Handler m G.Viewer
resetBranch G.ResetBranchInput{..} = do
  (_, branch') <- requireBranchAccess branch BranchAdmin
  -- TODO: better handling when `to` is not found
  commit <- Git.commitFromLabel $ Just to
  sha <- Branch.reset branch' $ CommitSha $ Git.commitSha commit

  G.viewer $ Just sha

submitBranch :: MonadGraph m => GithubSettings -> G.BranchInput -> Handler m G.SubmitBranchResponse
submitBranch settings G.BranchInput{..} = do
  (_, b) <- requireBranchAccess branch BranchAdmin

  Github.openPullRequest settings b >>= \case
    Left e -> throwIO $ ValidationMessage e
    Right url ->
      return $ pure branch :<> pure url

approveBranch :: MonadGraph m => G.BranchInput -> Handler m G.Viewer
approveBranch G.BranchInput{..} = do
  base <- storeBaseBranch <$> getStore
  (user, master) <- requireBranchAccess base BranchAdmin
  (_, pr) <- requireBranchAccess branch BranchAdmin

  let
    merge = Branch.Merge
      { from = pr
      , into = master
      }
    meta = CommitMeta (entityVal user) $ "Merge " <> branch <> " into " <> base
  sha <- Branch.merge merge meta

  G.viewer $ Just sha

updateProperty :: MonadGraph m => G.PatchInput -> G.UpdatePropertyInput -> Handler m G.Viewer
updateProperty patch G.UpdatePropertyInput{..} = do
  (user, branch) <- checkPatch patch

  old <- Store.fetch branch uid
  let updated = old
        { propertyDescription = fromMaybe (propertyDescription old) description
        , propertyRefs        = maybe (propertyRefs old) citations references
        }
  (p, sha) <- Store.put branch user ("Update " <> propertyName updated) updated
  G.presentView $ View.build [] [p] [] [] $ Just $ Version sha

updateSpace :: MonadGraph m => G.PatchInput -> G.UpdateSpaceInput -> Handler m G.Viewer
updateSpace patch G.UpdateSpaceInput{..} = do
  (user, branch) <- checkPatch patch

  old <- Store.fetch branch uid
  -- TODO:
  -- - HKD and write a generic patch merge
  -- - skip updates if nothing has changed
  let updated = old
        { spaceDescription = fromMaybe (spaceDescription old) description
        , spaceRefs        = maybe (spaceRefs old) citations references
        }
  (s, sha) <- Store.put branch user ("Update " <> spaceName updated) updated
  G.presentView $ View.build [s] [] [] [] $ Just $ Version sha

updateTheorem :: (MonadGraph m, MonadLogger m)
              => G.PatchInput -> G.UpdateTheoremInput -> Handler m G.Viewer
updateTheorem patch G.UpdateTheoremInput{..} = do
  (user, branch) <- checkPatch patch

  old <- Theorem.fetch branch uid
  let updated = old
        { theoremDescription = fromMaybe (theoremDescription old) description
        , theoremRefs        = maybe (theoremRefs old) citations references
        }
  (t, sha) <- Theorem.put branch user ("Update " <> theoremName updated) (propertyId <$> updated)
  G.presentView $ View.build [] [] [] [t] $ Just $ Version sha

updateTrait :: (MonadGraph m, MonadLogger m)
            => G.PatchInput -> G.UpdateTraitInput -> Handler m G.Viewer
updateTrait patch G.UpdateTraitInput{..} = do
  (user, branch) <- checkPatch patch

  old <- Trait.fetch branch spaceId propertyId
  let meta = CommitMeta user $ "Update " <> traitName old
      updated = old
        { _traitDescription = fromMaybe (_traitDescription old) description
        , _traitRefs        = maybe (_traitRefs old) citations references
        , _traitSpace       = spaceId
        , _traitProperty    = propertyId
        }
  Trait.put branch meta updated >>= G.presentView

-- Helpers

citations :: [G.CitationInput] -> [Citation]
citations = map (\(G.CitationInput name ct ref) -> Citation name ct ref)

-- TODO: should this return a loader for the branch?
checkPatch :: MonadGraph m => G.PatchInput -> m (User, Branch)
checkPatch G.PatchInput{..} = do
  (user, b) <- requireBranchAccess branch BranchWrite
  currentSha <- Branch.headSha b
  unless (sha == currentSha) $
    throwIO $ ConflictError
      { expectedSha = sha
      , actualSha = currentSha
      }
  return $ (entityVal user, b)

requireBranchAccess :: MonadGraph m => BranchName -> BranchAccess -> m (Entity User, Branch)
requireBranchAccess name minLevel = do
  user   <- requireUser
  branch <- Branch.find name >>= \case
    Nothing -> notFound "Branch" name
    Just b  -> return b
  Branch.access user branch >>= \case
    Just access -> do
      unless (access >= minLevel) $
        throwIO $ BranchPermissionRequired (entityVal branch) minLevel (Just access)
      return (user, entityVal branch)
    _ -> throwIO $ BranchPermissionRequired (entityVal branch) minLevel Nothing