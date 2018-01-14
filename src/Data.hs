{-# LANGUAGE TemplateHaskell #-}
module Data
  ( initializeStore
  , storeMaster
  , fetchPullRequest
  , findParsed
  , makeId
  , slugify
  , updateBranch
  , updateView
  , viewDeductions
  ) where

import           Control.Monad.Logger (MonadLogger, logInfo)
import qualified Data.Map             as M
import qualified Data.Set             as S
import qualified Data.UUID            as UUID
import qualified Data.UUID.V4         as UUID
import           Git
import           System.Directory     (doesDirectoryExist)
import           System.Process       (callCommand)

import qualified Data.Loader as Loader
import qualified Logic      as L
import qualified View       as V

import qualified Page
import qualified Page.Theorem
import qualified Page.Trait

import Core
import qualified Data.Branch as Branch
import Data.Git    (openRepo, updateBranch, resolveCommittish)
import Data.Store
import Git.Libgit2 (runLgRepository)
import Util        (indexBy)

storeMaster :: MonadStore m
            => m (Either [Error] View)
storeMaster = do
  var <- storeLoader <$> getStore
  withMVar var $ \loader -> do
    view <- Loader.view loader
    return $ Right view

initializeRepo :: (MonadIO m, MonadLogger m) => FilePath -> m LgRepo
initializeRepo path = do
  $(logInfo) $ "Initializing repository at " ++ tshow path
  exists <- liftIO $ doesDirectoryExist path
  unless exists $ do
    -- Clone initial repository along with all remote branches. See
    -- https://stackoverflow.com/questions/67699
    $(logInfo) $ "Cloning initial repository"
    liftIO . callCommand $ "git clone --mirror https://github.com/pi-base/data.git " ++ path ++ "/.git && cd " ++ path ++ " && git config --bool core.bare false && git checkout master"
  liftIO $ openRepo path

initializeStore :: (MonadIO m, MonadBase IO m, MonadBaseControl IO m, MonadMask m, MonadLogger m)
                => FilePath -> Ref -> m Store
initializeStore path ref = do
  repo <- initializeRepo path
  (Just commit) <- runLgRepository repo $ resolveCommittish $ CommitRef ref
  mkStore repo ref commit

fetchPullRequest :: MonadIO m => FilePath -> m ()
fetchPullRequest path = liftIO $ do
  putStrLn $ "Pulling updates for repo"
  callCommand $ "cd " ++ path ++ " && git fetch origin"

makeId :: MonadIO m => Text -> m (Id a)
makeId prefix = do
  uuid <- liftIO UUID.nextRandom
  return . Id $ prefix <> UUID.toText uuid

slugify :: Text -> Text
slugify t = t -- TODO

writeView :: MonadStore m => View -> TreeT LgRepo m ()
writeView v = forM_ (viewPages v) $ \(path, contents) -> do
  blob <- lift $ createBlobUtf8 contents
  putBlob path blob

viewPages :: View -> [(TreeFilePath, Text)]
viewPages v = map (Page.write Page.Theorem.page) theorems
           <> map (Page.write Page.Trait.page)   traits
  where
    theorems = map (fmap propertyId) $ V.theorems v
    traits   = map (identifyTrait . fst) $ V.traits v

viewDeductions :: MonadStore m
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

updateView :: MonadStore m
           => Branch
           -> CommitMeta
           -> (Loader.Loader -> m View)
           -> m View
updateView branch meta getView = do
  (view', sha) <- updateBranch branch meta $ \loader -> do
    v <- lift $ getView loader
    writeView v
    return v
  return $ view' { _viewVersion = Just $ Version sha }

addProof :: (TraitId, (TVal, L.Evidence))
         -> (Map SpaceId (Map PropertyId (Trait SpaceId PropertyId)), Map (SpaceId, PropertyId) Proof)
         -> (Map SpaceId (Map PropertyId (Trait SpaceId PropertyId)), Map (SpaceId, PropertyId) Proof)
addProof ((sid, pid), (value, evidence)) (traits, proofs) =
  let trait = Trait sid pid value ""
      proofs' = case evidence of
        L.Asserted -> proofs
        L.Deduced thrms props -> M.insert (sid, pid) (Proof sid props thrms) proofs
  in (insertNested sid pid trait traits, proofs')

insertNested :: (Ord k1, Ord k2) => k1 -> k2 -> v -> Map k1 (Map k2 v) -> Map k1 (Map k2 v)
insertNested k1 k2 v = M.alter (Just . M.insert k2 v . maybe mempty id) k1

findParsed :: MonadStore m
           => (Commit LgRepo -> a -> m (Either e b))
           -> Branch
           -> a
           -> m (Maybe b)
findParsed parser branch _id = do
  commit <- Branch.commit branch
  parsed <- parser commit _id
  return $ either (const Nothing) Just parsed