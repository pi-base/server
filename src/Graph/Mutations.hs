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

import Graph.Import

import           Control.Lens     (view)
import           Database.Persist (Entity(..))

import qualified Auth
import           Data.Store        (getDefaultBranch)
import qualified Data.Branch       as Branch
import qualified Data.Branch.Merge as Branch
import qualified Data.Git          as Git
import qualified Data.Id           as Id
import qualified Data.Property     as Property
import qualified Data.Space        as Space
import qualified Data.Theorem      as Theorem
import qualified Data.Trait        as Trait
import qualified Graph.Queries     as G
import qualified Graph.Schema      as G
import           Graph.Types       as G
import qualified Services.Github   as Github
import           Util              (slugify)
import qualified View              as View

mutations :: Graph m => Handler m G.MutationRoot
mutations =
  pure $ createSpace
     :<> createProperty
     :<> updateSpace
     :<> updateProperty
     :<> updateTheorem
     :<> updateTrait
     :<> assertTrait
     :<> assertTheorem
     :<> resetBranch
     :<> submitBranch
     :<> approveBranch
     :<> createUser

assertTrait :: Graph m => G.PatchInput -> G.AssertTraitInput -> Handler m G.Viewer
assertTrait patch G.AssertTraitInput{..} = do
  (user, branch) <- checkPatch patch

  space    <- Space.fetch branch spaceId
  property <- Property.fetch branch propertyId
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
  Trait.put branch commit trait' >>= G.presentView

assertTheorem :: (Graph m, MonadLogger m)
              => G.PatchInput -> G.AssertTheoremInput -> Handler m G.Viewer
assertTheorem patch G.AssertTheoremInput{..} = do
  (user, branch) <- checkPatch patch

  let theorem = Theorem
        { theoremId          = Id.pending
        , theoremImplication = (Implication antecedent consequent)
        , theoremConverse    = Nothing -- TODO
        , theoremRefs        = citations $ fromMaybe [] references
        , theoremDescription = fromMaybe "" description
        }
  theorem' <- mapM (Property.fetch branch) theorem
  let meta = CommitMeta user $ "Add " <> theoremName theorem'
  (t, sha) <- Theorem.put branch meta theorem
  G.presentView $ View.build [] [] [] [t] $ Just $ Version sha

createSpace :: Graph m => G.PatchInput -> G.CreateSpaceInput -> Handler m G.Viewer
createSpace patch G.CreateSpaceInput{..} = do
  (user, branch) <- checkPatch patch

  let space = Space
        { spaceId          = Id.pending
        , spaceName        = name
        , spaceAliases     = []
        , spaceRefs        = citations $ fromMaybe [] references
        , spaceTopology    = Nothing
        , spaceDescription = fromMaybe "" description
        }
      commit = CommitMeta user $ "Add " <> name

  (s, sha) <- Space.put branch commit space
  G.presentView $ View.build [s] [] [] [] $ Just $ Version sha

createProperty :: Graph m => G.PatchInput -> G.CreatePropertyInput -> Handler m G.Viewer
createProperty patch G.CreatePropertyInput{..} = do
  (user, branch) <- checkPatch patch

  let property = Property
        { propertyId          = Id.pending
        , propertyName        = name
        , propertyAliases     = []
        , propertyRefs        = citations $ fromMaybe [] references
        , propertyDescription = fromMaybe "" description
        }
      commit = CommitMeta user $ "Add " <> name
  (p, sha) <- Property.put branch commit property
  G.presentView $ View.build [] [p] [] [] $ Just $ Version sha

resetBranch :: Graph m => G.ResetBranchInput -> Handler m G.Viewer
resetBranch G.ResetBranchInput{..} = do
  (_, branch') <- requireBranchAccess branch BranchAdmin
  -- TODO: better handling when `to` is not found
  commit <- Git.commitFromLabel $ Just to
  sha <- Branch.reset branch' $ CommitSha $ Git.commitSha commit

  G.viewer $ Just sha

submitBranch :: Graph m => G.BranchInput -> Handler m G.SubmitBranchResponse
submitBranch G.BranchInput{..} = do
  (_, b) <- requireBranchAccess branch BranchAdmin

  ghs <- view githubSettings <$> getContext
  result <- Github.openPullRequest ghs b
  case result of
    Right url -> return $ pure branch :<> pure url
    Left e    -> throwIO $ ValidationMessage e

approveBranch :: Graph m => G.BranchInput -> Handler m G.Viewer
approveBranch G.BranchInput{..} = do
  defaultBranch <- getDefaultBranch
  (user, master) <- requireBranchAccess defaultBranch BranchAdmin
  (_, pr) <- requireBranchAccess branch BranchAdmin

  let
    merge = Branch.Merge
      { from = pr
      , into = master
      }
    meta = CommitMeta (entityVal user) $ "Merge " <> branch <> " into " <> defaultBranch
  sha <- Branch.merge merge meta

  G.viewer $ Just sha

updateProperty :: Graph m => G.PatchInput -> G.UpdatePropertyInput -> Handler m G.Viewer
updateProperty patch G.UpdatePropertyInput{..} = do
  (user, branch) <- checkPatch patch

  old <- Property.fetch branch uid
  let updated = old
        { propertyDescription = fromMaybe (propertyDescription old) description
        , propertyRefs        = maybe (propertyRefs old) citations references
        }
      commit  = CommitMeta user $ "Update " <> propertyName updated
  (p, sha) <- Property.put branch commit updated
  G.presentView $ View.build [] [p] [] [] $ Just $ Version sha

updateSpace :: Graph m => G.PatchInput -> G.UpdateSpaceInput -> Handler m G.Viewer
updateSpace patch G.UpdateSpaceInput{..} = do
  (user, branch) <- checkPatch patch

  old <- Space.fetch branch uid
  -- TODO:
  -- - HKD and write a generic patch merge
  -- - skip updates if nothing has changed
  let updated = old
        { spaceDescription = fromMaybe (spaceDescription old) description
        , spaceRefs        = maybe (spaceRefs old) citations references
        }
      meta = CommitMeta user $ "Update " <> spaceName updated
  (s, sha) <- Space.put branch meta updated
  G.presentView $ View.build [s] [] [] [] $ Just $ Version sha

updateTheorem :: (Graph m, MonadLogger m)
              => G.PatchInput -> G.UpdateTheoremInput -> Handler m G.Viewer
updateTheorem patch G.UpdateTheoremInput{..} = do
  (user, branch) <- checkPatch patch

  old <- Theorem.fetch branch uid
  let updated = old
        { theoremDescription = fromMaybe (theoremDescription old) description
        , theoremRefs        = maybe (theoremRefs old) citations references
        }
      meta    = CommitMeta user $ "Update " <> theoremName updated
  (t, sha) <- Theorem.put branch meta (propertyId <$> updated)
  G.presentView $ View.build [] [] [] [t] $ Just $ Version sha

updateTrait :: (Graph m, MonadLogger m)
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

createUser :: (Graph m, MonadLogger m)
           => G.CreateUserInput -> Handler m G.CreateUserResponse
createUser G.CreateUserInput{..} = do
  testing <- view G.testMode <$> getContext
  unless testing $ throwIO NotAuthenticated

  let user = User
        { userName       = name
        , userEmail      = slugify name <> "@example.com"
        , userIsReviewer = fromMaybe False reviewer
        }
  id <- Auth.ensureIdent "test" name name user
  token <- Auth.generateToken id

  let euser = Entity id user
  void $ Branch.ensureUserBranch euser

  return $ pure (tokenUuid token)

-- Helpers

citations :: [G.CitationInput] -> [Citation]
citations = map (\(G.CitationInput name ct ref) -> Citation name ct ref)

-- TODO: should this return a loader for the branch?
checkPatch :: Graph m => G.PatchInput -> m (User, Branch)
checkPatch G.PatchInput{..} = do
  (user, b) <- requireBranchAccess branch BranchWrite
  currentSha <- Branch.headSha b
  case sha of
    Just expected -> unless (expected == currentSha) $
      throwIO $ ConflictError
        { expectedSha = expected
        , actualSha = currentSha
        }
    _ -> return ()
  return $ (entityVal user, b)

requireBranchAccess :: Graph m => BranchName -> BranchAccess -> m (Entity User, Branch)
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