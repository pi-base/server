module Handler.Util
  ( deleteDerivedTraits
  , writeProofs
  ) where

import Git

import qualified Data.Map as M

import Import
import Core hiding (Handler)
import Data
import Data.Git (modifyGitRef)
import Logic (deduceTraits)
import qualified Page.Parser
import qualified Page.Trait as PT
import Util (indexBy)
import Viewer

deleteDerivedTraits :: Text -> Handler ()
deleteDerivedTraits ref = withViewerAt ref $ \Viewer{..} -> do
  putStrLn $ "Parsed viewer @" <> viewerVersion
  user <- systemUser
  void . modifyGitRef user ref "Delete derived traits" $ do
    let traits = indexBy traitId viewerTraits
        Proofs proofs = viewerProofs
        deduced = catMaybes $ map (\_id -> M.lookup _id traits) $ M.keys proofs
    putStrLn $ "Deleting " <> (tshow $ length deduced) <> " traits"
    forM_ deduced $ \trait -> dropEntry $ PT.path trait
  putStrLn "Done"

writeProofs :: Text -> Handler ()
writeProofs ref = withViewerAt ref $ \v@Viewer{..} -> do
  putStrLn $ "Deriving from " <> (tshow $ length viewerTraits) <> " traits."
  let proofs = deduceTraits v
  when (length proofs > 0) $ do
    putStrLn $ "Found " <> (tshow $ length proofs) <> " proofs. Writing to disk."
    user <- systemUser
    void . modifyGitRef user ref "Add deduced traits" $ do
      forM_ proofs $ \(trait, evidence) -> do
        let (path, contents) = Page.Parser.write $ PT.write (trait, Just evidence)
        (lift $ createBlobUtf8 contents) >>= putBlob path

-- TODO: pull from system .gitconfig if present
systemUser :: Handler User
systemUser = return $ User "jamesdabbs" "James Dabbs" "jamesdabbs@gmail.com" ""

withViewerAt :: (MonadIO m, MonadTrans t, MonadStore (t m))
             => Text
             -> (Viewer -> t m ())
             -> t m ()
withViewerAt ref f = viewerAtRef ref >>= \case
  Left errors  -> mapM_ (lift . putStrLn . explainError) errors
  Right viewer -> f viewer
