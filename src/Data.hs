module Data
  ( mkStore
  , storeMaster
  , parseViewer
  , viewerAtRef
  , fetchPullRequest
  , makeId
  , slugify
  , updateView
  , bridgeLoader
  , viewDeductions
  ) where

import           Conduit         (sourceToList)
import qualified Data.Map        as M
import qualified Data.Set        as S
import qualified Data.UUID       as UUID
import qualified Data.UUID.V4    as UUID
import           Git
import           System.Directory (doesDirectoryExist)
import           System.Process   (callCommand)

import qualified Data.Parse as P
import qualified Logic      as L
import qualified View       as V

import qualified Page
import qualified Page.Theorem
import qualified Page.Trait

import Core
import Data.Git (openRepo, useRef)
import Util     (indexBy)

storeMaster :: MonadStore m
            => m (Either [Error] View)
storeMaster = do
  base <- storeBaseRef <$> getStore
  storeCached . parseViewer $ CommitRef base

initializeRepo :: MonadIO m => FilePath -> m LgRepo
initializeRepo path = liftIO $ do
  putStrLn $ "Initializing repository at " ++ tshow path
  exists <- doesDirectoryExist path
  unless exists $ do
    putStrLn "Cloning initial repository"
    callCommand $ "git clone https://github.com/pi-base/data.git " ++ path
  openRepo path

fetchPullRequest :: MonadIO m => FilePath -> m ()
fetchPullRequest path = liftIO $ do
  putStrLn $ "Pulling updates for repo"
  callCommand $ "cd " ++ path ++ " && git fetch origin"

viewerAtRef :: MonadStore m => Text -> m (Either [Error] View)
viewerAtRef = parseViewer . CommitRef . Ref

parseViewer :: MonadStore m
            => Committish
            -> m (Either [Error] View)
parseViewer commish = do
  eviewer <- P.viewer commish
  -- validate
  return eviewer

makeId :: MonadIO m => (Text -> a) -> Text -> m a
makeId constructor prefix = do
  uuid <- liftIO UUID.nextRandom
  return . constructor $ prefix <> UUID.toText uuid

slugify :: Text -> Text
slugify t = t -- TODO

updateView :: MonadStore m
           => Ref
           -> CommitMeta
           -> TreeT LgRepo m (Either Error View)
           -> m (Either Error View)
updateView ref meta updates = do
  results <- useRef ref meta $ do
    updates >>= \case
      Left   err -> return $ Left err
      Right view -> do
        persistView view
        return $ Right view
  case results of
    Left e -> return $ Left e
    Right (version, view') -> return . Right $ view' { _viewVersion = Just version }

persistView :: MonadStore m => View -> TreeT LgRepo m ()
persistView v = forM_ (viewPages v) $ \(path, contents) -> do
  blob <- lift $ createBlobUtf8 contents
  putBlob path blob

viewPages :: View -> [(TreeFilePath, Text)]
viewPages v = map (Page.write Page.Theorem.page) theorems
           <> map (Page.write Page.Trait.page)   traits
  where
    theorems = map (fmap propertyId) $ V.theorems v
    traits   = map (identifyTrait . fst) $ V.traits v

mkStore :: FilePath -> Ref -> IO Store
mkStore path baseRef = Store
  <$> initializeRepo path
  <*> newMVar Nothing
  <*> pure baseRef

storeCached :: MonadStore m
            => m (Either a View)
            -> m (Either a View)
storeCached f = do
  Store{..} <- getStore
  modifyMVar storeCache $ \mev -> case mev of
    Just viewer -> return $ (Just viewer, Right viewer)
    _ -> f >>= \case
      Left err     -> return $ (Nothing, Left err)
      Right viewer -> return $ (Just viewer, Right viewer)

bridgeLoader :: Monad m => CLoader m -> Loader m
bridgeLoader CLoader{..} = Loader
  { loaderImplications = do
      theorems <- sourceToList $ clTheorems Nothing
      return $ Right $ map (\t -> (theoremId t, theoremImplication t)) theorems
  , loaderSpaceIds = do
      spaces <- sourceToList $ clSpaces Nothing
      return $ Right $ map spaceId spaces
  , loaderSpace = \_id -> clTraits _id
  }

viewDeductions :: Monad m
               => CLoader m -> L.Deductions -> m (Either Error View)
viewDeductions loader L.Deductions{..} = runExceptT $ do
  let (sids, pids) = foldr (\(s,p) (ss,ps) -> (S.insert s ss, S.insert p ps)) (mempty, mempty) $ M.keys deductionTraits

  spaces <- lift $ sourceToList $ clSpaces loader $ Just sids
  let _viewSpaces = indexBy spaceId spaces

  let pids' = foldr (S.union . theoremProperties) pids deductionTheorems
  properties <- lift $ sourceToList $ clProperties loader $ Just pids'
  let _viewProperties = indexBy propertyId properties

  let _viewTheorems = indexBy theoremId deductionTheorems
      (_viewTraits, _viewProofs) = foldr addProof (mempty, mempty) $ M.toList deductionTraits

  let _viewVersion = Nothing

  return View{..}

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
