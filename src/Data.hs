module Data
  ( Store
  , MonadStore(..)
  , Viewer(..)
  , Committish(..)
  , mkStore
  -- TODO: refactor vvv to use Data.Parse
  , storeMaster
  , parseViewer
  , viewerAtRef
  , fetchPullRequest
  --
  , findProperty
  , findSpace
  , findTheorem
  , createProperty
  , createSpace
  , updateProperty
  , updateSpace
  , updateTheorem
  , getSpaceDescription
  , getPropertyDescription
  , getTheoremDescription
  ) where

import qualified Data.ByteString.Char8 as BS
import qualified Data.Map.Strict       as M
import qualified Data.Text             as T

import Control.Monad.Trans.State.Strict (modify', get, runStateT)
import Data.Aeson                       (FromJSON)
import Data.Either.Combinators          (mapLeft)
import Data.List                        (nub)
import Data.Tagged                      (Tagged(..))
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID
import Git                              hiding (Object)
import Git.Libgit2                      (LgRepo)
import System.Process                   (callCommand)

import qualified Page
import qualified Page.Parser
import qualified Page.Property
import qualified Page.Space
import qualified Page.Theorem
import qualified Page.Trait

import Core
import Data.Git
import Model (User)
import Util
import Viewer
import qualified Viewer as V

type V m = StateT (Viewer, [Error]) (ReaderT LgRepo m) ()

-- TODO: replace with Data.Parse.viewer
storeMaster :: MonadStore m
            => m (Either [Error] Viewer)
storeMaster = storeCached $ parseViewer (Ref "master")

fetchPullRequest :: MonadIO m => FilePath -> m ()
fetchPullRequest path = liftIO $ do
  putStrLn $ "Pulling updates for repo"
  callCommand $ "cd " ++ path ++ " && git fetch origin"

viewerAtRef :: MonadStore m => Text -> m (Either [Error] Viewer)
viewerAtRef = parseViewer . Ref

parseAt :: MonadStore m
        => Committish
       -> a
       -> (Commit LgRepo -> StateT (a, [Error]) (ReaderT LgRepo m) (Maybe Error))
       -> m (Either [Error] a)
parseAt commish initial f = useRepo . gatherErrors initial $ do
  eref <- lift $ lookupCommitish commish
  case eref of
    Nothing  -> return . Just $ CommitNotFound commish
    Just ref -> (lift . lookupCommit $ Tagged ref) >>= f

parseViewer :: MonadStore m
            => Committish
            -> m (Either [Error] Viewer)
parseViewer commish = parseAt commish V.empty $ \cmt -> do
  modify' $ \(v,e) -> (v { viewerVersion = commitVersion cmt }, e)

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

getSpaceDescription :: MonadStore m
                    => Space -> m Text
getSpaceDescription Space{..} = return spaceDescription

getPropertyDescription :: MonadStore m
                       => Property -> m Text
getPropertyDescription Property{..} = return propertyDescription

getTheoremDescription :: MonadStore m
                      => Theorem Property -> m Text
getTheoremDescription Theorem{..} = return theoremDescription

record :: (FromJSON f, Monad m)
   => (Page.Parser.Page f -> Either Error a) -- parse
   -> (a -> Either Error b)                  -- hydrate
   -> (Viewer -> b -> Viewer)                -- insert
   -> Record
   -> StateT (Viewer, [Error]) (ReaderT LgRepo m) ()
record parse hydrate insert r =
  case Page.Parser.parse r >>= parse >>= hydrate of
    Left err  -> addError err
    Right obj -> modify' $ \(v, es) -> (insert v obj, es)

addProperty :: Monad m => Record -> V m
addProperty = record Page.Property.parse return $
  \v p -> v { viewerProperties = p : viewerProperties v }

addSpace :: Monad m => Record -> V m
addSpace = record Page.Space.parse return $
  \v s -> v { viewerSpaces = s : viewerSpaces v }

addTheorem :: Monad m
           => M.Map Text Property
           -> Record
           -> V m
addTheorem props r@(path, _) = record Page.Theorem.parse hydrate insert r
  where
    hydrate = mapLeft (ReferenceError path) . hydrateTheorem props
    insert v t = v { viewerTheorems = t : viewerTheorems v}

addTrait :: MonadIO m
         => M.Map Text Space
         -> M.Map Text Property
         -> Record
         -> V m
addTrait spaces props r@(path, _) = case Page.Parser.parse r >>= Page.Trait.parse of
  Left err -> addError err
  Right (trait, mproof) -> case hydrateTrait path spaces props trait of
    Left err -> addError err
    Right ht -> do
      modify' . withViewer $ recordTrait ht
      modify' . withViewer $ recordProof ht mproof

withViewer :: (Viewer -> Viewer) -> (Viewer, a) -> (Viewer, a)
withViewer f (v,x) = (f v,x)

recordTrait :: Trait Space Property -> Viewer -> Viewer
recordTrait t v = v { viewerTraits = t : viewerTraits v }

recordProof :: Trait Space Property -> Maybe Assumptions -> Viewer -> Viewer
recordProof t (Just p) v = v { viewerProofs = insertProof (traitSpaceId t, traitPropertyId t) p $ viewerProofs v }
recordProof _ Nothing v = v

-- insertProof :: TraitId -> Assumptions -> Proofs -> Proofs
insertProof = M.insert

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

hydrateTrait :: TreeFilePath -> Map Uid s -> Map Uid p -> Trait Uid Uid -> Either Error (Trait s p)
hydrateTrait path ss ps t@Trait{..} = case (M.lookup traitSpace ss, M.lookup traitProperty ps) of
  (Just s, Just p)   -> Right $ t { traitSpace = s, traitProperty = p }
  (Nothing, Nothing) -> Left $ ReferenceError path [traitSpace, traitProperty]
  (Nothing, _) -> Left $ ReferenceError path [traitSpace]
  (_, Nothing) -> Left $ ReferenceError path [traitProperty]

gatherErrors :: Monad m => a -> StateT (a, [Error]) m (Maybe Error) -> m (Either [Error] a)
gatherErrors v s = do
  (ret, (viewer, errors)) <- runStateT s (v, [])
  return $ case ret of
    Just err -> Left $ err : errors
    Nothing  -> if null errors
      then Right viewer
      else Left $ nub errors

updateSpace :: MonadStore m
            => User -> Space -> Text -> m (Maybe Space)
updateSpace user space description = useRepo $ do
    let updated = space { spaceDescription = description }
    writeContents user ("Updated " <> spaceName space)
      [Page.Parser.write $ Page.Space.write space]
    return $ Just updated

makeId :: MonadStore m => m Text
makeId = do
  uuid <- liftIO UUID.nextRandom
  return $ UUID.toText uuid

slugify :: Text -> Text
slugify t = t

createSpace :: MonadStore m
            => User -> Text -> Text -> m Space
createSpace user name description = useRepo $ do
  _id <- makeId
  let space = Space (SpaceId $ "s" <> _id) (slugify name) name description Nothing
  writeContents user ("Add " <> name)
    [Page.Parser.write $ Page.Space.write space]
  return space

createProperty :: MonadStore m
               => User -> Text -> Text -> m Property
createProperty user name description = useRepo $ do
  _id <- makeId
  let property = Property (PropertyId $ "p" <> _id) (slugify name) name Nothing description
  writeContents user ("Add " <> name)
    [Page.Parser.write $ Page.Property.write property]
  return property

findSpace :: MonadStore m => SpaceId -> m (Maybe Space)
findSpace _id = storeMaster >>= \case
  Left _ -> return Nothing
  Right Viewer{..} -> return $ find (\Space{..} -> spaceId == _id) viewerSpaces

updateProperty :: MonadStore m
            => User -> Property -> Text -> m (Maybe Property)
updateProperty user property description = useRepo $ do
  let updated = property { propertyDescription = description }
  writeContents user ("Updated " <> propertyName property)
    [Page.Parser.write $ Page.Property.write updated]
  return $ Just updated

findProperty :: MonadStore m => PropertyId -> m (Maybe Property)
findProperty _id = storeMaster >>= \case
  Left _ -> return Nothing
  Right Viewer{..} -> return $ find (\Property{..} -> propertyId == _id) viewerProperties

updateTheorem :: MonadStore m
              => User -> Theorem Property -> Text -> m (Maybe (Theorem Property))
updateTheorem user theorem description = useRepo $ do
  let updated = theorem { theoremDescription = description }
  writeContents user ("Updated " <> theoremName theorem)
    [Page.Parser.write $ Page.Theorem.write updated]
  return $ Just updated

findTheorem :: MonadStore m => TheoremId -> m (Maybe (Theorem Property))
findTheorem _id = storeMaster >>= \case
  Left _ -> return Nothing
  Right Viewer{..} -> return $ find (\Theorem{..} -> theoremId == _id) viewerTheorems

theoremName :: Theorem Property -> Text
theoremName = T.pack . show . theoremImplication
