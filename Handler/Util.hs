module Handler.Util
  ( deleteDerivedTraits
  , migrateReferences
  , writeProofs
  ) where

import Import
import Core hiding (Handler)

import qualified Data.Aeson.Types as Aeson
import qualified Data.Map         as M

import Data
import Data.Git (modifyGitRef, updateRef, useRepo, writePages)
import qualified Data.Git as Git
import Git

import qualified Data.Parse  as P
import qualified Logic       as L
import qualified View        as V

import qualified Page
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
      convertTrait (path, contents) = undefined
        -- trait <- parse page
        -- space    <- fetch ss $ traitSpace trait
        -- property <- fetch ps $ traitSpace trait
        -- let updated = trait { traitSpace = space, traitProperty = property }
        --     newContents = writePage updated
        --     newPath     = replace (traitSpace trait) space
        --                 . replace (traitProperty trait) property $ path

        -- lift (createBlobUtf8 newContents) >>= putBlob newPath
        -- dropEntry path

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
