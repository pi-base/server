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

import           Core hiding (Handler)
import           Data.Git as Git
import           Formula (Formula)
import qualified Graph.Loader as GL
import qualified Graph.Types as G
import           Handler.Helpers (requireToken)
import           Model (User(..))

user :: G G.User
user = do
  (Entity _id User{..}) <- requireToken
  return $ pure "User" :<> pure userName

viewer :: Maybe Text -> G G.Viewer
viewer mver = do
  commit <- Git.commitFromLabel mver
  loader <- GL.mkLoader commit -- TODO: cache loader for base branch
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
presentSpace s@Space{..} traits = pure $ pure "Space"
  :<> pure (unSpaceId spaceId)
  :<> pure spaceSlug
  :<> pure spaceName
  :<> pure spaceDescription
  :<> pure spaceTopology
  :<> pure (map presentTrait traits)

presentProperty :: Monad m => Property -> Handler m G.Property
presentProperty Property{..} = pure $ pure "Property"
  :<> pure (unPropertyId propertyId)
  :<> pure propertySlug
  :<> pure propertyName
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

presentView :: Monad m => View -> Handler m G.Viewer
presentView = error "presentView"
-- viewR View{..} = pure $ pure "Viewer"
--   :<> pure (maybe "" unVersion _viewVersion)
--   :<> spacesR     sha (M.elems _viewSpaces    ) _viewTraits _viewProperties
--   :<> (pure . map propertyR $ M.elems _viewProperties)
--   :<> theoremsR   sha (M.elems _viewTheorems  ) (findKey _viewProperties)
--   where
--     sha = CommitSha . unVersion <$> _viewVersion

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
  :<> pure spaceDescription
  :<> pure spaceTopology
  :<> loadTraits
  where
    loadTraits = do
      traits <- GL.spaceTraits loader s
      return $ map presentTrait traits

encodeFormula :: Formula PropertyId -> Text
encodeFormula = TL.toStrict . decodeUtf8 . Data.Aeson.encode . map unPropertyId
