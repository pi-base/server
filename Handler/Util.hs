module Handler.Util where

import Data.Tagged
import Data.Time.LocalTime (getZonedTime)
import Git

import qualified Data.Map as M

import Import
import Core hiding (Handler)
import Data
import Data.Git
import qualified Page.Trait as PT
import Util (indexBy)
import Viewer

deleteDerivedTraits :: Handler ()
deleteDerivedTraits = do
  store <- getStore
  eviewer <- parseViewer store (Ref "audit")
  case eviewer of
    Left errors  -> mapM_ (lift . putStrLn . explainError) errors
    Right Viewer{..} -> useRepo store $ do
      putStrLn $ "Parsed viewer @" <> viewerVersion
      let
        Proofs proofs = viewerProofs
        deduced = M.keys proofs
        traits  = indexBy traitId viewerTraits
        refname = "refs/heads/audit"

      mref <- resolveReference refname
      (parent, tree) <- case mref of
        Nothing  -> error "Could not find user branch"
        Just ref -> do
          parent <- lookupCommit $ Tagged ref
          tree   <- lookupTree $ commitTree parent
          return (parent, tree)

      (_, newTree) <- withTree tree $ do
        forM_ deduced $ \traitId -> case M.lookup traitId traits of
          Nothing -> return ()
          Just trait -> dropEntry $ PT.path trait

      time <- lift $ lift getZonedTime
      let sig = defaultSignature
           { signatureEmail = "jamesdabbs@gmail.com"
           , signatureName  = "James Dabbs"
           , signatureWhen  = time
           }
      let message = "Delete auto-generated traits"
      createCommit [commitOid parent] newTree sig sig message (Just refname)
      return ()
  putStrLn "Done"
