{-# LANGUAGE Rank2Types #-}
module Handler.Util
  ( deleteDerivedTraits
  , migrateReferences
  , writeProofs
  ) where

import Import
import Core hiding (Handler)

import qualified Data.Aeson.Types as Aeson
import qualified Data.Map         as M
import qualified Data.HashMap.Strict as HM

import Control.Lens hiding ((.=))
import Control.Lens.Traversal
import Data
import Data.Git (modifyGitRef, updateRef, useRepo, writePages)
import qualified Data.Git as Git
import Git
import Util (fetch)

import qualified Data.Parse  as P
import qualified Logic       as L
import qualified View        as V

import qualified Page
import qualified Page.Parser
import qualified Page.Property
import qualified Page.Space
import qualified Page.Trait

deleteDerivedTraits :: Text -> Handler ()
deleteDerivedTraits ref = withViewerAt ref $ \v@View{..} -> do
  putStrLn $ "Parsed viewer @" <> maybe "??" unVersion _viewVersion
  user <- systemUser
  void . modifyGitRef user (Ref ref) "Delete derived traits" $ do
    let deduced = catMaybes $ map (\(t, mp) -> maybe Nothing (const $ Just t) mp) $ V.traits v
    putStrLn $ "Deleting " <> (tshow $ length deduced) <> " traits"
    mapM_ (dropEntry . Page.Trait.path) deduced
  putStrLn "Done"

writeProofs :: Text -> Handler ()
writeProofs ref = withViewerAt ref $ \v@View{..} -> do
  putStrLn $ "Deriving from " <> (tshow $ length $ V.traits v) <> " traits."
  case L.updates v $ L.checkAllTraits of
    Left errs -> putStrLn $ "Errors running prover: " <> tshow errs
    Right updates -> do
      let pages = updatedPages updates
      putStrLn $ "Found " <> (tshow $ length pages) <> " updates. Writing to disk."
      user <- systemUser
      void . modifyGitRef user (Ref ref) "Add deduced traits" $ do
        forM_ pages $ \(path, contents) ->
          (lift $ createBlobUtf8 contents) >>= putBlob path

-- TODO
-- Updates the given git ref,
-- * rename spaces, properties and traits to use ids
-- * update space: and property: references in traits to use ids instead of slugs
migrateReferences :: Text -> Handler Version
migrateReferences ref = do
  commitMessage <- systemCommit "Update references and paths to use ids"

  P.at (CommitRef $ Ref ref) $ \commit -> do
    ss <- runConduit $ P.spaceEntries commit
       .| P.blobs
       .| mapC (Page.withFrontmatter $ \f -> do
                   uid  <- f .: "uid"
                   slug <- f .: "slug"
                   return (slug, uid))
       .| throwLeft
       .| P.sinkMap

    ps <- runConduit $ P.propertyEntries commit
       .| P.blobs
       .| mapC (Page.withFrontmatter $ \f -> do
                   uid  <- f .: "uid"
                   slug <- f .: "slug"
                   return (slug, uid))
       .| throwLeft
       .| P.sinkMap

    let
      convertTrait (path, contents) = do
        (newPath, newContents) <- lift $ updateTrait ss ps (path, contents)
        lift (createBlobUtf8 newContents) >>= putBlob newPath
        dropEntry path

    Git.updateRef (Ref ref) commitMessage $ do
      forM_ (M.toList ss) $ \(slug, uid) -> do
        Git.move (encodeUtf8 $ "spaces/" <> slug <> "/README.md")
                 (encodeUtf8 $ "spaces/" <> uid  <> "/README.md")

      forM_ (M.toList ps) $ \(slug, uid) -> do
        Git.move (encodeUtf8 $ "properties/" <> slug <> ".md")
                 (encodeUtf8 $ "properties/" <> uid  <> ".md")

      len <- runConduit $ (transPipe lift $ P.traitEntries commit .| P.blobs)
                       .| mapMC convertTrait
                       .| lengthC
      putStrLn $ "Updated " ++ tshow (len :: Int) ++ " traits"

type P = Page Aeson.Object
newtype Pager a = Pager (Prism' P a)

traitPage :: Pager (Trait Text Text)
traitPage = Pager $ prism toPage fromPage
  where
    toPage :: Trait Text Text -> P
    toPage Trait{..} = Page
      { pagePath = encodeUtf8 $ "spaces/" <> _traitSpace <> "/properties/" <> _traitProperty <> ".md"
      , pageFrontmatter = HM.fromList
        [ "space"    .= _traitSpace
        , "property" .= _traitProperty
        , "value"    .= _traitValue
        ]
      , pageMain = _traitDescription
      , pageSections = mempty
      }

    fromPage :: P -> Either P (Trait Text Text)
    fromPage p@Page{..} =
      mapLeft (const p) . flip Aeson.parseEither pageFrontmatter $ \f -> do
        _traitSpace    <- f .: "space"
        _traitProperty <- f .: "property"
        _traitValue    <- f .: "value"
        let _traitDescription = pageMain
        return Trait{..}

parsePage :: MonadThrow m => Pager a -> (TreeFilePath, Text) -> m a
parsePage (Pager p) (path, content) = case Page.Parser.parse (path, content) of
  Left err -> throwM err
  Right page -> case page ^? p of
    Just a  -> return a
    Nothing -> throwM $ ParseError path "does not define a valid object"

writePage :: Pager a -> a -> (TreeFilePath, Text)
writePage (Pager p) page = Page.Parser.write $ page ^. re p

updateTrait :: MonadThrow m
            => Map Text Text
            -> Map Text Text
            -> (TreeFilePath, Text)
            -> m (TreeFilePath, Text)
updateTrait ss ps (path, contents) = do
  trait   <- parsePage traitPage (path, contents)
  updated <- mapMOf traitProperty (flip fetch ps) =<< mapMOf traitSpace (flip fetch ss) trait
  return $ writePage traitPage (updated :: Trait Text Text)

throwLeft :: (Exception a, MonadThrow m) => ConduitM (Either a b) b m ()
throwLeft = awaitForever $ either throwM yield

-- TODO: pull from system .gitconfig if present
systemUser :: Handler User
systemUser = return $ User "jamesdabbs" "James Dabbs" "jamesdabbs@gmail.com" ""

systemCommit :: Text -> Handler CommitMeta
systemCommit message = CommitMeta <$> systemUser <*> pure message

withViewerAt :: (MonadIO m, MonadTrans t, MonadStore (t m))
             => Text
             -> (View -> t m ())
             -> t m ()
withViewerAt ref f = viewerAtRef ref >>= \case
  Left errors  -> mapM_ (lift . putStrLn . explainError) errors
  Right viewer -> f viewer
