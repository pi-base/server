module Handler.Util
  ( deleteDerivedTraits
  , migrateReferences
  , writeProofs
  ) where

import Import
import Core hiding (Handler)


import Control.Lens hiding ((.=))
import Data
import qualified Data.Aeson       as Aeson
import qualified Data.Aeson.Types as Aeson
import Data.Aeson.Lens
import Data.Git (modifyGitRef)
import qualified Data.Git  as Git
import qualified Data.Map  as M
import qualified Data.Text as T
import qualified Formula
import Git

import qualified Data.Parse  as P
-- import qualified Logic       as L
import qualified View        as V

import qualified Page

type Slug = Text

deleteDerivedTraits :: Text -> Handler ()
deleteDerivedTraits ref = withViewerAt ref $ \v@View{..} -> do
  putStrLn $ "Parsed viewer @" <> maybe "??" unVersion _viewVersion
  user <- systemUser
  void . modifyGitRef user (Ref ref) "Delete derived traits" $ do
    let deduced = catMaybes $ map (\(t, mp) -> maybe Nothing (const $ Just t) mp) $ V.traits v
    putStrLn $ "Deleting " <> (tshow $ length deduced) <> " traits"
    mapM_ (dropEntry . error "path") deduced
  putStrLn "Done"

writeProofs :: Text -> Handler ()
writeProofs ref = withViewerAt ref $ \v@View{..} -> do
  putStrLn $ "Deriving from " <> (tshow $ length $ V.traits v) <> " traits."
  error "writeProofs"
  -- case L.updates v $ L.checkAll of
  --   Left errs -> putStrLn $ "Errors running prover: " <> tshow errs
  --   Right updates -> do
  --     let pages = updatedPages updates
  --     putStrLn $ "Found " <> (tshow $ length pages) <> " updates. Writing to disk."
  --     user <- systemUser
  --     void . modifyGitRef user (Ref ref) "Add deduced traits" $ do
  --       forM_ pages $ \(path, contents) ->
  --         (lift $ createBlobUtf8 contents) >>= putBlob path

-- TODO
-- delete correct old paths when moving files
migrateReferences :: Text -> Handler Version
migrateReferences ref = do
  P.at (CommitRef $ Ref ref) $ \commit -> do
    ss <- runConduit $ P.spaceEntries commit
       .| P.blobs
       .| mapC (Page.withFrontmatter $ \f -> do
                   uid  <- SpaceId <$> f .: "uid"
                   slug <- f .: "slug"
                   return (slug, uid))
       .| throwLeftC
       .| sinkMap

    ps <- runConduit $ P.propertyEntries commit
       .| P.blobs
       .| mapC (Page.withFrontmatter $ \f -> do
                   uid  <- PropertyId <$> f .: "uid"
                   slug <- f .: "slug"
                   return (slug, uid))
       .| throwLeftC
       .| sinkMap

    let
      moveWith :: (MonadGit LgRepo m, MonadThrow m)
               => ((TreeFilePath, Text) -> Either Error (TreeFilePath, Text))
               -> (TreeFilePath, Text)
               -> TreeT LgRepo m ()
      moveWith f (path, contents) = case f (path, contents) of
        Left err -> lift $ throwM err
        Right (newPath, newContents) -> do
          lift (createBlobUtf8 newContents) >>= putBlob newPath
          unless (path == newPath) $ dropEntry path

    commitMessage <- systemCommit "Update references and paths to use ids"
    Git.updateRef (Ref ref) commitMessage $ do
      forM_ (M.toList ss) $ \(slug, uid) -> do
        Git.move (encodeUtf8 $ "spaces/" <> slug <> "/README.md")
                 (encodeUtf8 $ "spaces/" <> unSpaceId uid <> "/README.md")

      forM_ (M.toList ps) $ \(slug, uid) -> do
        Git.move (encodeUtf8 $ "properties/" <> slug <> ".md")
                 (encodeUtf8 $ "properties/" <> unPropertyId uid <> ".md")

      nTheorem <- runConduit $ (transPipe lift $ P.theoremEntries commit .| P.blobs)
                            .| mapMC (moveWith $ updateTheorem ps)
                            .| lengthC
      putStrLn $ "Updated " ++ tshow (nTheorem :: Int) ++ " theorems"

      nTrait <- runConduit $ (transPipe lift $ P.traitEntries commit .| P.blobs)
                          .| mapMC (moveWith $ updateTrait ss ps)
                          .| lengthC
      putStrLn $ "Updated " ++ tshow (nTrait :: Int) ++ " traits"

updateTrait :: Map Slug SpaceId
            -> Map Slug PropertyId
            -> (TreeFilePath, Text)
            -> Either Error (TreeFilePath, Text)
updateTrait ss ps = Page.updateMetadata $ \(_, meta) -> do
  pid <- fmap unPropertyId . lookupEither ps $ meta ^. key "property" . _String
  sid <- fmap unSpaceId    . lookupEither ss $ meta ^. key "space"    . _String
  let meta' = meta & key "space" .~ String sid & key "property" .~ String pid
      path' = encodeUtf8 $ "spaces/" <> sid <> "/properties/" <> pid <> ".md"
  return (path', meta')

updateTheorem :: Map Slug PropertyId
              -> (TreeFilePath, Text)
              -> Either Error (TreeFilePath, Text)
updateTheorem ps = Page.updateMetadata $ \(_, meta) -> do
  meta' <- process "if" meta >>= process "then"
  let path' = encodeUtf8 $ "theorems/" <> (meta ^. key "uid" . _String) <> ".md"
  return (path', meta')
  where
    process field = mapMOf (key field . _Value) h

    h :: Value -> Either String Value
    h v = do
      f  <- Aeson.parseEither Aeson.parseJSON v
      f' <- mapLeft (T.unpack . T.intercalate ", ") $ Formula.hydrate ps f
      return $ toJSON f'

lookupEither :: (Show k, Ord k) => Map k v -> k -> Either String v
lookupEither m k = case M.lookup k m of
  Just v -> Right v
  Nothing -> Left $ show k

throwLeftC :: (Exception a, MonadThrow m) => ConduitM (Either a b) b m ()
throwLeftC = awaitForever $ either throwM yield

-- TODO: pull from system .gitconfig if present
systemUser :: MonadIO m => m User
systemUser = return $ User "jamesdabbs" "James Dabbs" "jamesdabbs@gmail.com" ""

systemCommit :: MonadIO m => Text -> m CommitMeta
systemCommit message = CommitMeta <$> systemUser <*> pure message

withViewerAt :: (MonadIO m, MonadTrans t, MonadStore (t m))
             => Text
             -> (View -> t m ())
             -> t m ()
withViewerAt ref f = viewerAtRef ref >>= \case
  Left errors  -> mapM_ (lift . putStrLn . explainError) errors
  Right viewer -> f viewer

sinkMap :: (Ord a, Monad m) => ConduitM (a,b) Void m (Map a b)
sinkMap = sinkList >>= return . M.fromList
