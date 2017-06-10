module Handler.Util
  ( deleteDerivedTraits
  , writeProofs
  ) where

import Import
import Core hiding (Handler)

import Data
import Data.Git (modifyGitRef)
import Git

import qualified Data.Parse
import qualified Logic       as L
import qualified Page.Trait  as PT
import qualified View        as V

deleteDerivedTraits :: Text -> Handler ()
deleteDerivedTraits ref = withViewerAt ref $ \v@View{..} -> do
  putStrLn $ "Parsed viewer @" <> maybe "??" unVersion _viewVersion
  user <- systemUser
  void . modifyGitRef user (Ref ref) "Delete derived traits" $ do
    let deduced = catMaybes $ map (\(t, mp) -> maybe Nothing (const $ Just t) mp) $ V.traits v
    putStrLn $ "Deleting " <> (tshow $ length deduced) <> " traits"
    mapM_ (dropEntry . PT.path) deduced
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

-- TODO: pull from system .gitconfig if present
systemUser :: Handler User
systemUser = return $ User "jamesdabbs" "James Dabbs" "jamesdabbs@gmail.com" ""

withViewerAt :: (MonadIO m, MonadTrans t, MonadStore (t m))
             => Text
             -> (View -> t m ())
             -> t m ()
withViewerAt ref f = viewerAtRef ref >>= \case
  Left errors  -> mapM_ (lift . putStrLn . explainError) errors
  Right viewer -> f viewer
