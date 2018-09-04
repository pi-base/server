module Data.Trait
  ( find
  , fetch
  , put
  ) where

import Protolude hiding (find, put)

import           Core
import           Data          (required, updateView)
import qualified Data.Branch   as Branch
import qualified Data.Loader   as Load
import qualified Data.Parse    as Parse
import qualified Data.Storable as Store
import qualified View

find :: (MonadStore m, MonadLogger m)
     => Branch
     -> SpaceId
     -> PropertyId
     -> m (Maybe (Trait Space Property))
find branch sid pid = do
  tree     <- Branch.tree branch
  parsed   <- Parse.trait tree sid pid
  space    <- Store.find branch sid
  property <- Store.find branch pid
  return $ Trait
    <$> space
    <*> property
    <*> Just (_traitValue parsed)
    <*> Just (_traitRefs parsed)
    <*> Just (_traitDescription parsed)

fetch :: (MonadStore m, MonadLogger m)
      => Branch
      -> SpaceId
      -> PropertyId
      -> m (Trait Space Property)
fetch branch sid pid =
  Data.Trait.find branch sid pid >>= Data.required "Trait" (show (sid, pid))

put :: (MonadStore m, MonadLogger m)
    => Branch
    -> CommitMeta
    -> Trait SpaceId PropertyId
    -> m View
put branch meta trait' = updateView branch meta $ \loader -> do
  -- TODO: for now, we're only going to validate for assertion conflicts
  --   on the frontend and at review / merge time
  -- result <- L.runLogicT loader $ L.assertTrait trait
  -- case result of
  --   Left      err -> throw $ LogicError err
  --   Right updates -> viewDeductions loader $ updates
  space    <- Load.space    loader $ _traitSpace    trait'
  property <- Load.property loader $ _traitProperty trait'
  let trait = trait' { _traitSpace = space, _traitProperty = property }
  return $ View.build [space] [property] [trait] [] Nothing