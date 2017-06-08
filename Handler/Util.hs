module Handler.Util
  ( deleteDerivedTraits
  , writeProofs
  , newParse
  ) where

import Git

import qualified Data.Map as M

import Import
import Core hiding (Handler)
import Data
import Data.Git (modifyGitRef)
import qualified Logic as L
import qualified Page.Parser
import qualified Page.Trait as PT
import Util (indexBy)

import qualified Data.Parse

deleteDerivedTraits :: Text -> Handler ()
deleteDerivedTraits ref = withViewerAt ref $ \View{..} -> do
  putStrLn $ "Parsed viewer @" <> maybe "??" unVersion _viewVersion
  user <- systemUser
  void . modifyGitRef user ref "Delete derived traits" $ do
    fixme "deleteDerivedTraits"
    -- let traits = indexBy traitId viewerTraits
    --     deduced = catMaybes $ map (\_id -> M.lookup _id traits) $ M.keys viewerProofs
    -- putStrLn $ "Deleting " <> (tshow $ length deduced) <> " traits"
    -- forM_ deduced $ \trait -> dropEntry $ PT.path trait
  putStrLn "Done"

writeProofs :: Text -> Handler ()
writeProofs ref = withViewerAt ref $ \v@View{..} -> do
  -- putStrLn $ "Deriving from " <> (tshow $ length viewTraits) <> " traits."
  case L.updates v $ L.checkAllTraits of
    Left errs     -> putStrLn $ "Errors running prover: " <> tshow errs
    Right updates -> do
      let pages = updatedPages updates
      putStrLn $ "Found " <> (tshow $ length pages) <> " updates. Writing to disk."
      user <- systemUser
      fixme "write deduced proofs to specified branch"
      -- void . modifyGitRef user ref "Add deduced traits" $ do
      --   forM_ proofs $ \(trait, evidence) -> do
      --     let (path, contents) = Page.Parser.write $ PT.write (trait, Just evidence)
      --     (lift $ createBlobUtf8 contents) >>= putBlob path

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

newParse = Data.Parse.viewer $ Ref "audit"
