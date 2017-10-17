{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
module Graph.Query
  ( presentView
  , user
  , viewer
  ) where

import Graph.Import

import qualified Data.Aeson
import qualified Data.Text.Lazy as TL
import           Database.Persist.Types (Entity(..))

import           Core            hiding (Handler)
import           Data.Git        as Git
import qualified Data.Map        as M
import           Formula         (Formula)
import qualified Graph.Loader    as GL
import qualified Graph.Types     as G
import           Handler.Helpers (requireToken)
import           Model           (User(..))

user :: G G.User
user = do
  (Entity _id User{..}) <- requireToken
  return $ pure "User" :<> pure userName

viewer :: Maybe Text -> G G.Viewer
viewer mver = do
  commit <- Git.commitFromLabel mver
  loader <- GL.mkLoader commit
  return $ pure "Viewer"
    :<> pure (unVersion $ GL.version loader)
    :<> loadSpaces loader
    :<> loadProperties loader
    :<> loadTheorems loader

-- Pure presenters (unify with m ~ Identity)
presentSpace :: Monad m
             => Space
             -> [Trait s Property]
             -> Handler m G.Space
presentSpace Space{..} traits = pure $ pure "Space"
  :<> pure (unSpaceId spaceId)
  :<> pure spaceSlug
  :<> pure spaceName
  :<> pure (map pure spaceAliases)
  :<> pure spaceDescription
  :<> pure spaceTopology
  :<> pure (map presentTrait traits)

presentProperty :: Monad m => Property -> Handler m G.Property
presentProperty Property{..} = pure $ pure "Property"
  :<> pure (unPropertyId propertyId)
  :<> pure propertySlug
  :<> pure propertyName
  :<> pure (map pure propertyAliases)
  :<> pure propertyDescription

presentTrait :: Monad m
             => Trait s Property
             -> Handler m G.Trait
presentTrait t@Trait{..} = pure $ pure "Trait"
  :<> presentProperty _traitProperty
  :<> pure _traitValue

presentTheorem :: Monad m => Theorem PropertyId -> Handler m G.Theorem
presentTheorem t@Theorem{..} = pure $ pure "Theorem"
  :<> pure (unTheoremId theoremId)
  :<> pure (encodeFormula $ theoremIf t)
  :<> pure (encodeFormula $ theoremThen t)
  :<> pure theoremDescription

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
               => GL.Loader
               -> Handler m (List G.Property)
loadProperties loader = do
  properties <- GL.allProperties loader
  return $ map presentProperty properties

loadTheorems :: MonadStore m
             => GL.Loader
             -> Handler m (List G.Theorem)
loadTheorems loader = do
  theorems <- GL.allTheorems loader
  return $ map presentTheorem theorems

loadSpaces :: MonadStore m
           => GL.Loader
           -> Handler m (List G.Space)
loadSpaces loader = do
  spaces <- GL.allSpaces loader
  return $ map (loadSpace loader) spaces

loadSpace :: MonadStore m
          => GL.Loader
          -> Space
          -> Handler m G.Space
loadSpace loader s@Space{..} = pure $ pure "Space"
  :<> pure (unSpaceId spaceId)
  :<> pure spaceSlug
  :<> pure spaceName
  :<> pure (map pure spaceAliases)
  :<> pure spaceDescription
  :<> pure spaceTopology
  :<> loadTraits loader s

loadTraits :: MonadStore m => GL.Loader -> Space -> Handler m (List G.Trait)
loadTraits loader space = do
  traits <- GL.spaceTraits loader space
  return . map (loadTrait loader space) $ M.toList traits

loadTrait :: MonadStore m => GL.Loader -> Space -> (PropertyId, TVal) -> Handler m G.Trait
loadTrait loader space (pid, tval) = pure $ pure "Trait"
  :<> (GL.loadProperty loader pid >>= presentProperty)
  :<> pure tval

encodeFormula :: Formula PropertyId -> Text
encodeFormula = TL.toStrict . decodeUtf8 . Data.Aeson.encode . map unPropertyId
