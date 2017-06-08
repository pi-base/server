module View
  ( theorems
  , traits
  ) where

import Core
import Control.Lens

import qualified Data.Map as M

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
validate = fixme "validate"
  -- validate that space and property ids and slugs are unique
  -- note: if _ids_ aren't unique, we presumably already lost them while building the view
  -- so we probably need to add validations for id uniqueness during parsing
