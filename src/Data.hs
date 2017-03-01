module Data
  ( Store
  , Viewer(..)
  , Committish(..)
  , mkStore
  , storeMaster
  , fetchPullRequest
  , parseProperty
  , parseSpace
  , parseViewer
  ) where

import qualified Data.ByteString.Char8 as BS
import qualified Data.Map.Strict       as M
import qualified Data.Text             as T

import Control.Monad.Catch              (MonadMask)
import Control.Monad.Trans.State.Strict (modify', get, runStateT)
import Data.ByteString                  (ByteString)
import Data.Either.Combinators          (mapLeft)
import Data.Tagged                      (Tagged(..))
import Git                              hiding (Object)
import Git.Libgit2                      (LgRepo)
import System.Process                   (callCommand)


import Core
import Data.Git
import Data.Parsers
import Util
import Viewer
import qualified Viewer as V

data Committish = Ref Text | Sha Text

type V m = StateT (Viewer, [Error]) (ReaderT LgRepo m) ()

lookupCommitish :: MonadGit r m => Committish -> m (Maybe (Oid r))
lookupCommitish (Ref ref) = resolveReference $ "refs/heads/" <> ref
lookupCommitish (Sha sha) = Just <$> parseOid sha


storeMaster :: (MonadIO m, MonadMask m, MonadBaseControl IO m)
            => Store -> m (Either [Error] Viewer)
storeMaster store = storeCached store $ \s -> parseViewer s (Ref "master")

fetchPullRequest :: MonadIO m => FilePath -> m ()
fetchPullRequest path = liftIO $ do
  putStrLn $ "Pulling updates for repo"
  callCommand $ "cd " ++ path ++ " && git fetch origin"

parseViewer :: (MonadIO m, MonadMask m, MonadBaseControl IO m)
            => Store
            -> Committish
            -> m (Either [Error] Viewer)
parseViewer store ref = useRepo store . gatherErrors $ do
  eref <- lift $ lookupCommitish ref
  case eref of
    Nothing -> return . Just $ NotATree "check your branch name"
    Just ref -> do
      cmt <- lift $ lookupCommit $ Tagged ref
      case commitOid cmt of
        (Tagged oid) -> modify' $ \(v,e) -> (v { viewerVersion = T.pack $ show oid }, e)

      tree <- lift $ lookupTree $ commitTree cmt

      lift (eachBlob tree "properties") >>= mapM_ addProperty

      (v, _) <- get
      let propsBySlug = indexBy propertySlug $ viewerProperties v
      lift (eachBlob tree "spaces") >>= mapM_ (\(path, blob) ->
        if "README.md" `BS.isSuffixOf` path
          then addSpace (path, blob)
          else return ())

      (v', _) <- get
      let spacesBySlug = indexBy spaceSlug $ viewerSpaces v'
      lift (eachBlob tree "spaces") >>= mapM_ (\(path, blob) ->
        if "README.md" `BS.isSuffixOf` path
          then return ()
          else addTrait spacesBySlug propsBySlug (path, blob))

      lift (eachBlob tree "theorems") >>= mapM_ (addTheorem propsBySlug)

      validate

      return Nothing

record :: Monad m
   => (Record -> Either Error a) -- parse
   -> (a -> Either Error b)      -- hydrate
   -> (Viewer -> b -> Viewer)    -- insert
   -> Record
   -> StateT (Viewer, [Error]) (ReaderT LgRepo m) ()
record parse hydrate insert r =
  case parse r >>= hydrate of
    Left err  -> addError err
    Right obj -> modify' $ \(v, es) -> (insert v obj, es)

addProperty :: Monad m => Record -> V m
addProperty = record parseProperty return $
  \v p -> v { viewerProperties = p : viewerProperties v }

addSpace :: Monad m => Record -> V m
addSpace = record parseSpace return $
  \v s -> v { viewerSpaces = s : viewerSpaces v }

addTheorem :: Monad m
           => M.Map Text Property
           -> Record
           -> V m
addTheorem props = record parseTheorem hydrate $
  \v t -> v { viewerTheorems = t : viewerTheorems v}
  where
    hydrate = mapLeft (ReferenceError "theorem") . hydrateTheorem props

addTrait :: MonadIO m
         => M.Map Text Space
         -> M.Map Text Property
         -> Record
         -> V m
addTrait spaces props r = case parseTrait r of
  Left err -> addError err
  Right (trait, mproof) -> case hydrateTrait spaces props trait of
    Left err -> addError err
    Right ht -> do
      modify' . withViewer $ recordTrait ht
      modify' . withViewer $ recordProof ht mproof

withViewer :: (Viewer -> Viewer) -> (Viewer, a) -> (Viewer, a)
withViewer f (v,x) = (f v,x)

recordTrait :: Trait Space Property -> Viewer -> Viewer
recordTrait t v = v { viewerTraits = t : viewerTraits v }

recordProof :: Trait Space Property -> Maybe [Assumption] -> Viewer -> Viewer
recordProof Trait{..} (Just p) v = v { viewerProofs = insertProof traitId p $ viewerProofs v }
recordProof _ Nothing v = v

insertProof :: TraitId -> [Assumption] -> Proofs -> Proofs
insertProof _id p (Proofs map) = Proofs $ M.insert _id p map

addError :: Monad m => Error -> V m
addError e = modify' $ \(v, es) -> (v, e : es)

validate :: (MonadIO m, Monad m) => V m
validate = do
  (Viewer{..}, _) <- get
  -- TODO: add other validators
  -- verifyUnique "Space ID" spaceId viewerSpaces
  -- verifyUnique "Property ID" propertyId viewerProperties
  verifyUnique "Space slug" spaceSlug viewerSpaces
  verifyUnique "Property slug" propertySlug viewerProperties

  where
    verifyUnique label f coll = mapM_ (addError . NotUnique label) . dupes $ map f coll

hydrateTrait :: Map Uid s -> Map Uid p -> Trait Uid Uid -> Either Error (Trait s p)
hydrateTrait ss ps t@Trait{..} = case (M.lookup traitSpace ss, M.lookup traitProperty ps) of
  (Just s, Just p)   -> Right $ t { traitSpace = s, traitProperty = p }
  (Nothing, Nothing) -> Left $ ReferenceError "trait" [traitSpace, traitProperty]
  (Nothing, _) -> Left $ ReferenceError "space" [traitSpace]
  (_, Nothing) -> Left $ ReferenceError "property" [traitProperty]

gatherErrors :: Monad m => StateT (Viewer, [Error]) m (Maybe Error) -> m (Either [Error] Viewer)
gatherErrors s = do
  (ret, (viewer, errors)) <- runStateT s (V.empty, [])
  return $ case ret of
    Just err -> Left $ err : errors
    Nothing  -> if null errors
      then Right viewer
      else Left errors

