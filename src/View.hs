module View
  ( build
  , theorems
  , traits
  ) where

import Protolude
import Core
import Control.Lens

import qualified Data.Map as M

import Util (groupBy, indexBy)

theorems :: View -> [Theorem Property]
theorems v = M.foldr' acc [] $ v ^. viewTheorems
  where
    props :: Map PropertyId Property
    props = v ^. viewProperties

    acc :: Theorem PropertyId -> [Theorem Property] -> [Theorem Property]
    acc t ts = case hydrateTheorem props t of
      Left  _  -> ts
      Right t' -> t' : ts

traits :: View -> [(Trait Space Property, Maybe Proof)]
traits v = foldr acc [] $ flattened
  where
    flattened :: [Trait SpaceId PropertyId]
    flattened = join . M.elems . M.map M.elems $ v ^. viewTraits

    acc :: Trait SpaceId PropertyId
        -> [(Trait Space Property, Maybe Proof)]
        -> [(Trait Space Property, Maybe Proof)]
    acc t ts =
      let
        sid   = t ^. traitSpace
        pid   = t ^. traitProperty
        ms    = M.lookup sid $ v ^. viewSpaces
        mp    = M.lookup pid $ v ^. viewProperties
        proof = M.lookup (sid, pid) $ v ^. viewProofs
      in
        case (ms, mp) of
          (Just s, Just p) -> ((set traitSpace s . set traitProperty p $ t), proof) : ts
          _ -> ts

build :: [Space]
      -> [Property]
      -> [Trait Space Property]
      -> [Theorem Property]
      -> Maybe Version
      -> View
build ss ps ts is version = View
  { _viewSpaces     = indexBy spaceId ss
  , _viewProperties = indexBy propertyId ps
  , _viewTraits     = M.map (indexBy _traitProperty) $ groupBy _traitSpace $ map identifyTrait ts
  , _viewTheorems   = indexBy theoremId $ map (fmap propertyId) is
  , _viewProofs     = mempty
  , _viewVersion    = version
  }
