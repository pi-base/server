module View
  ( build
  , theorems
  , traits
  ) where

import Core hiding (groupBy)
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
        sid   = traitSpace t
        pid   = traitProperty t
        ms    = M.lookup sid $ v ^. viewSpaces
        mp    = M.lookup pid $ v ^. viewProperties
        proof = M.lookup (sid, pid) $ v ^. viewProofs
      in
        case (ms, mp) of
          (Just s, Just p) -> (t { traitSpace = s, traitProperty = p }, proof) : ts
          _ -> ts

validate :: View -> [Error]
validate v = [] -- TODO

build :: [Space]
      -> [Property]
      -> [Trait Space Property]
      -> [Theorem Property]
      -> Version
      -> View
build ss ps ts is version = View
  { _viewSpaces     = indexBy spaceId ss
  , _viewProperties = indexBy propertyId ps
  , _viewTraits     = M.map (indexBy traitProperty) $ groupBy traitSpace $ map identifyTrait ts
  , _viewTheorems   = indexBy theoremId $ map (fmap propertyId) is
  , _viewProofs     = mempty
  , _viewVersion    = Just version
  }
