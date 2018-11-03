module Data.Trait
  ( fetch
  , put
  ) where

import Core hiding (find, put)

import           Data  (required, updateView)
import qualified Data.Branch as Branch
import qualified Data.Parse as Parse
import qualified Data.Property
import qualified Data.Space
import qualified Data.Loader as Load
import qualified View

find :: Git m
     => Branch
     -> SpaceId
     -> PropertyId
     -> m (Maybe (Trait Space Property))
find branch sid pid = do
  tree     <- Branch.tree branch
  parsed   <- Parse.trait tree sid pid
  space    <- Data.Space.find branch sid
  property <- Data.Property.find branch pid
  return $ Trait
    <$> space
    <*> property
    <*> Just (_traitValue parsed)
    <*> Just (_traitRefs parsed)
    <*> Just (_traitDescription parsed)

fetch :: Git m
      => Branch
      -> SpaceId
      -> PropertyId
      -> m (Trait Space Property)
fetch branch sid pid =
  Data.Trait.find branch sid pid >>= Data.required "Trait" (show (sid, pid))

put :: (Git m, MonadLogger m)
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