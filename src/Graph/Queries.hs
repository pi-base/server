{-#
  LANGUAGE
    TypeApplications
  , TypeOperators
#-}
module Graph.Queries
  ( presentView
  , user
  , viewer
  ) where

import Graph.Import hiding (readFile)

import qualified Data.Aeson
import qualified Data.Text.Lazy as TL
import           Database.Persist.Types (Entity(..))

import           Core         hiding (readFile)
import           Data.Branch  (userBranches)
import           Data.Loader  (Loader)
import qualified Data.Loader  as Loader
import qualified Data.Map     as M
import qualified Data.Store
import           Formula      (Formula)
import qualified Graph.Schema as G
import           Model        (User(..))
import           Types        (BranchAccess(..))

user :: MonadGraph m => Handler m G.User
user = do
  u@(Entity _id User{..}) <- requireUser
  return $ pure "User"
    :<> pure userName
    :<> do
      branches <- userBranches u
      pure (map presentBranch branches)

viewer :: MonadGraph m => Maybe Text -> Handler m G.Viewer
viewer mver = do
  loader <- maybe Data.Store.currentLoader Data.Store.loaderAt mver
  return $ pure "Viewer"
    :<> pure (unVersion $ Loader.version loader)
    :<> loadSpaces loader
    :<> loadProperties loader
    :<> loadTheorems loader

-- Pure presenters (unify with m ~ Identity)
presentBranch :: Monad m => BranchStatus -> Handler m G.Branch
presentBranch BranchStatus{..} = pure $ pure "Branch"
  :<> pure (branchName branch)
  :<> pure (serializeAccess branchAccess)
  :<> pure branchHead
  where
    serializeAccess BranchRead  = "read"
    serializeAccess BranchWrite = "write"
    serializeAccess BranchAdmin = "admin"

presentSpace :: Monad m
             => Space
             -> [Trait s Property]
             -> Handler m G.Space
presentSpace Space{..} traits = pure $ pure "Space"
  :<> pure (unId spaceId)
  :<> pure spaceSlug
  :<> pure spaceName
  :<> pure (map pure spaceAliases)
  :<> pure (map presentCitation spaceRefs)
  :<> pure spaceDescription
  :<> pure spaceTopology
  :<> pure (map presentTrait traits)

presentProperty :: Monad m => Property -> Handler m G.Property
presentProperty Property{..} = pure $ pure "Property"
  :<> pure (unId propertyId)
  :<> pure propertySlug
  :<> pure propertyName
  :<> pure (map pure propertyAliases)
  :<> pure (map presentCitation propertyRefs)
  :<> pure propertyDescription

presentTrait :: Monad m
             => Trait s Property
             -> Handler m G.Trait
presentTrait Trait{..} = pure $ pure "Trait"
  :<> presentProperty _traitProperty
  :<> pure _traitValue
  :<> pure (map presentCitation _traitRefs)
  :<> pure _traitDescription

presentTheorem :: Monad m => Theorem PropertyId -> Handler m G.Theorem
presentTheorem t@Theorem{..} = pure $ pure "Theorem"
  :<> pure (unId theoremId)
  :<> pure (encodeFormula $ theoremIf t)
  :<> pure (encodeFormula $ theoremThen t)
  :<> pure (map presentCitation theoremRefs)
  :<> pure theoremDescription

presentCitation :: Monad m => Citation -> Handler m G.Citation
presentCitation Citation{..} = pure $ pure "Citation"
  :<> pure 
    ( case citationType of
        DOICitation  -> "doi"
        MRCitation   -> "mr"
        WikiCitation -> "wikipedia"
    )
  :<> pure citationRef
  :<> pure citationName

-- IDEA: define View -> PureLoader and re-use?
presentView :: Monad m => View -> Handler m G.Viewer
presentView View{..} = pure $ pure "Viewer"
  :<> pure (maybe "" unVersion _viewVersion)
  :<> pure (map presentSpace' $ M.toList _viewSpaces)
  :<> pure (map presentProperty $ M.elems _viewProperties)
  :<> pure (map presentTheorem  $ M.elems _viewTheorems)
  where
    spaceTraits :: SpaceId -> [Trait SpaceId Property]
    spaceTraits _id = case M.lookup _id _viewTraits of
      Nothing -> []
      Just traits -> catMaybes $ map lookupProperty $ M.elems traits

    lookupProperty :: Trait s PropertyId -> Maybe (Trait s Property)
    lookupProperty t = do
      prop <- M.lookup (_traitProperty t) _viewProperties
      return $ t { _traitProperty = prop }

    presentSpace' :: Monad m => (SpaceId, Space) -> Handler m G.Space
    presentSpace' (_id, space) = presentSpace space (spaceTraits _id)

-- Cached loader based handlers
loadProperties :: MonadStore m
               => Loader
               -> Handler m (List G.Property)
loadProperties loader = do
  properties <- Loader.properties loader
  return $ map presentProperty properties

loadTheorems :: MonadStore m
             => Loader
             -> Handler m (List G.Theorem)
loadTheorems loader = do
  theorems <- Loader.theorems loader
  return $ map presentTheorem theorems

loadSpaces :: MonadStore m
           => Loader
           -> Handler m (List G.Space)
loadSpaces loader = do
  spaces <- Loader.spaces loader
  return $ map (loadSpace loader) spaces

loadSpace :: MonadStore m
          => Loader.Loader
          -> Space
          -> Handler m G.Space
loadSpace loader s@Space{..} = pure $ pure "Space"
  :<> pure (unId spaceId)
  :<> pure spaceSlug
  :<> pure spaceName
  :<> pure (map pure spaceAliases)
  :<> pure (map presentCitation spaceRefs)
  :<> pure spaceDescription
  :<> pure spaceTopology
  :<> loadTraits loader s

loadTraits :: MonadStore m => Loader -> Space -> Handler m (List G.Trait)
loadTraits loader space = do
  traits <- Loader.spaceTraits loader $ spaceId space
  return . map (loadTrait loader space) $ M.toList traits

loadTrait :: MonadStore m => Loader -> Space -> (PropertyId, TVal) -> Handler m G.Trait
loadTrait loader space (pid, tval) = do
  trait <- Loader.trait loader (spaceId space) pid
  return $ pure "Trait"
    :<> (Loader.property loader pid >>= presentProperty)
    :<> pure tval
    :<> pure (map presentCitation $ _traitRefs trait)
    :<> pure (_traitDescription trait)

encodeFormula :: Formula PropertyId -> Text
encodeFormula = TL.toStrict . decodeUtf8 . Data.Aeson.encode . map unId
