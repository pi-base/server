{-# LANGUAGE TemplateHaskell #-}
module Data
  ( findParsed
  , Data.required
  , Data.updateBranch
  , updateView
  , viewDeductions
  ) where

import Core

import           Control.Lens         (view)
import           Control.Monad.Logger (MonadLogger)
import qualified Data.Map             as M
import qualified Data.Set             as S
import           Git

import qualified Data.Loader as Loader
import qualified Logic      as L
import qualified View       as V

import qualified Page
import qualified Page.Theorem
import qualified Page.Trait

import qualified Data.Branch as Branch
import           Data.Git    as Git (updateBranch, writePages)
import           Data.Store  as Store
import           Util        (indexBy)

viewPages :: View -> [(TreeFilePath, Text)]
viewPages v = map (Page.write Page.Theorem.page) theorems
           <> map (Page.write Page.Trait.page)   traits
  where
    -- Only write asserted traits (where proof is null)
    traits   = map (identifyTrait . fst) $ filter (isNothing . snd) $ V.traits v
    theorems = map (fmap propertyId) $ V.theorems v

viewDeductions :: Git m
               => Loader.Loader -> L.Deductions -> m View
viewDeductions loader L.Deductions{..} = do
  let (sids, pids) = foldr (\(s,p) (ss,ps) -> (S.insert s ss, S.insert p ps)) (mempty, mempty) $ M.keys deductionTraits

  spaces <- forM (S.toList sids) $ Loader.space loader
  let _viewSpaces = indexBy spaceId spaces

  let pids' = foldr (S.union . theoremProperties) pids deductionTheorems
  properties <- forM (S.toList pids') $ Loader.property loader
  let _viewProperties = indexBy propertyId properties

  let _viewTheorems = indexBy theoremId deductionTheorems
      (_viewTraits, _viewProofs) = foldr addProof (mempty, mempty) $ M.toList deductionTraits

  let _viewVersion = Nothing

  return View{..}

updateBranch :: (Git m, MonadLogger m)
             => Branch
             -> CommitMeta
             -> (Loader.Loader -> TreeT LgRepo m a)
             -> m (a, Sha)
updateBranch branch meta handler = do
  result <- Git.updateBranch (branchName branch) meta handler
  push   <- view (storeSettings . autoPush) <$> getStore
  when push $ background $ pushBranch branch
  return result

updateView :: (Git m, MonadLogger m)
           => Branch
           -> CommitMeta
           -> (Loader.Loader -> m View)
           -> m View
updateView branch meta getView = do
  (view', sha) <- Data.updateBranch branch meta $ \loader -> do
    v <- lift $ getView loader
    Git.writePages $ viewPages v
    return v
  return $ view' { _viewVersion = Just $ Version sha }

addProof :: (TraitId, (TVal, L.Evidence))
         -> (Map SpaceId (Map PropertyId (Trait SpaceId PropertyId)), Map (SpaceId, PropertyId) Proof)
         -> (Map SpaceId (Map PropertyId (Trait SpaceId PropertyId)), Map (SpaceId, PropertyId) Proof)
addProof ((sid, pid), (value, evidence)) (traits, proofs) =
  let trait = Trait sid pid value [] ""
      proofs' = case evidence of
        L.Asserted -> proofs
        L.Deduced thrms props -> M.insert (sid, pid) (Proof sid props thrms) proofs
  in (insertNested sid pid trait traits, proofs')

insertNested :: (Ord k1, Ord k2) => k1 -> k2 -> v -> Map k1 (Map k2 v) -> Map k1 (Map k2 v)
insertNested k1 k2 v = M.alter (Just . M.insert k2 v . maybe mempty identity) k1

findParsed :: Git m
           => (Tree LgRepo -> a -> m b)
           -> Branch
           -> a
           -> m (Maybe b)
findParsed parser branch id = handle fail $ do
  commit <- Branch.commit branch
  tree   <- lookupTree $ commitTree commit
  parsed <- parser tree id
  return $ Just parsed
  where
    fail :: Monad m => NotFoundError -> m (Maybe b)
    fail _ = return Nothing

required :: MonadIO m => Text -> Text -> Maybe a -> m a
required resource identifier =
  maybe (notFound resource identifier) return

-- FIXME: this should enqueue work in a persistent queue, retry, ...
background :: Data m => m () -> m ()
background action = do
  unlift <- askUnliftIO
  void . liftIO . forkIO . unliftIO unlift $ void action
