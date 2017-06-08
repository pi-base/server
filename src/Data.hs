module Data
  ( mkStore
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
  , assertTrait
  , assertTheorem
  ) where

import           Control.Lens
import qualified Data.Map.Strict as M
import qualified Data.Text       as T
import qualified Data.UUID       as UUID
import qualified Data.UUID.V4    as UUID
import           System.Process  (callCommand)

import qualified Data.Parse as P
import qualified Logic      as L

import qualified Page.Parser
import qualified Page.Property
import qualified Page.Space
import qualified Page.Theorem
import qualified Page.Trait

import Core
import Data.Git (openRepo, useRepo, writeContents)
import Util     (fetch, indexBy)

storeMaster :: MonadStore m
            => m (Either [Error] View)
storeMaster = storeCached $ parseViewer (Ref "master")

fetchPullRequest :: MonadIO m => FilePath -> m ()
fetchPullRequest path = liftIO $ do
  putStrLn $ "Pulling updates for repo"
  callCommand $ "cd " ++ path ++ " && git fetch origin"

viewerAtRef :: MonadStore m => Text -> m (Either [Error] View)
viewerAtRef = parseViewer . Ref

parseViewer :: MonadStore m
            => Committish
            -> m (Either [Error] View)
parseViewer commish = do
  eviewer <- P.viewer commish
  -- validate
  return eviewer

getSpaceDescription :: MonadStore m
                    => Space -> m Text
getSpaceDescription Space{..} = return spaceDescription

getPropertyDescription :: MonadStore m
                       => Property -> m Text
getPropertyDescription Property{..} = return propertyDescription

getTheoremDescription :: MonadStore m
                      => Theorem p -> m Text
getTheoremDescription Theorem{..} = return theoremDescription

validate :: View -> Either [Error] View
validate = error "validate"
  -- TODO: add other validators
  -- verifyUnique "Space ID" spaceId viewerSpaces
  -- verifyUnique "Property ID" propertyId viewerProperties
  -- verifyUnique "Space slug" spaceSlug viewerSpaces
  -- verifyUnique "Property slug" propertySlug viewerProperties

  -- where
  --   verifyUnique label f coll = mapM_ (addError . NotUnique label) . dupes $ map f coll

updateSpace :: MonadStore m
            => User -> Space -> Text -> m (Maybe Space)
updateSpace user space description = useRepo $ do
    let updated = space { spaceDescription = description }
    writeContents user ("Updated " <> spaceName space)
      [Page.Parser.write $ Page.Space.write space]
    return $ Just updated

-- makeId :: MonadStore m => m Text
makeId cons prefix = do
  uuid <- liftIO UUID.nextRandom
  return . cons $ prefix <> UUID.toText uuid

slugify :: Text -> Text
slugify t = t

createSpace :: MonadStore m
            => User -> Text -> Text -> m Space
createSpace user name description = useRepo $ do
  _id <- makeId SpaceId "s"
  let space = Space _id (slugify name) name description Nothing
  writeContents user ("Add " <> name)
    [Page.Parser.write $ Page.Space.write space]
  return space

createProperty :: MonadStore m
               => User -> Text -> Text -> m Property
createProperty user name description = useRepo $ do
  _id <- makeId PropertyId "p"
  let property = Property _id (slugify name) name Nothing description
  writeContents user ("Add " <> name)
    [Page.Parser.write $ Page.Property.write property]
  return property

userBranchRef :: User -> Committish
userBranchRef User{..} = Ref $ "users/" <> userName

assertTrait :: MonadStore m
            => User -> SpaceId -> PropertyId -> Bool -> Text -> m (Either [Error] View)
assertTrait user sid pid value description = do
  let branch = userBranchRef user
  P.viewSpace sid branch >>= \case
    Left errs -> return $ Left errs
    Right v -> do
      trait <- buildTrait v sid pid value description
      case L.updates v $ L.assertTrait trait of
        Left err -> return $ Left [LogicError err]
        Right updates -> persistUpdates updates user $ "Add " <> traitName trait

-- TODO: dedup this w/ assertTrait
assertTheorem :: MonadStore  m
              => User -> Formula PropertyId -> Formula PropertyId -> Text -> m (Either [Error] View)
assertTheorem user ant con desc = do
  let branch = userBranchRef user
  P.viewer branch >>= \case
    Left errs -> return $ Left errs
    Right v -> do
      theorem <- buildTheorem v ant con desc
      traceM $ show theorem
      case L.updates v $ L.assertTheorem theorem of
        Left err      -> return $ Left [LogicError err]
        Right updates -> persistUpdates updates user $ "Add " <> theoremName theorem

buildTheorem :: MonadStore m
             => View -> Formula PropertyId -> Formula PropertyId -> Text -> m (Theorem Property)
buildTheorem v ant con desc = do
  _id <- makeId TheoremId "t"
  let dehyrdated = Theorem _id (Implication ant con) Nothing desc
  case hydrateTheorem (v ^. viewProperties) dehyrdated of
    Left errs -> throwM . NotFound $ tshow errs
    Right t   -> return t

buildTrait :: MonadThrow m
           => View -> SpaceId -> PropertyId -> Bool -> Text -> m (Trait Space Property)
buildTrait View{..} sid pid value description = do
  space    <- fetch sid _viewSpaces
  property <- fetch pid _viewProperties
  return $ Trait space property value description

persistUpdates :: MonadStore m => View -> User -> Text -> m (Either [Error] View)
persistUpdates v user message = useRepo $ do
  let
    pages =  map (Page.Parser.write . Page.Theorem.write) (fullTheorems v)
          <> map (Page.Parser.write . Page.Trait.write)   (fullTraits   v)

  writeContents user message pages >>= \case
    Left      msg -> return . Left  $ [PersistError msg]
    Right version -> return . Right $ v { _viewVersion = Just version }

findMaster f _id = storeMaster >>= \case
  Left  _ -> return Nothing
  Right v -> return . M.lookup _id $ f v

findSpace :: MonadStore m => SpaceId -> m (Maybe Space)
findSpace = findMaster _viewSpaces

updateProperty :: MonadStore m
            => User -> Property -> Text -> m (Maybe Property)
updateProperty user property description = useRepo $ do
  let updated = property { propertyDescription = description }
  writeContents user ("Updated " <> propertyName property)
    [Page.Parser.write $ Page.Property.write updated]
  return $ Just updated

findProperty :: MonadStore m => PropertyId -> m (Maybe Property)
findProperty = findMaster _viewProperties

updateTheorem :: MonadStore m
              => User -> Theorem Property -> Text -> m (Maybe (Theorem Property))
updateTheorem user theorem description = useRepo $ do
  let updated = theorem { theoremDescription = description }
  writeContents user ("Updated " <> theoremName theorem)
    [Page.Parser.write $ Page.Theorem.write updated]
  return $ Just updated

findTheorem :: MonadStore m => TheoremId -> m (Maybe (Theorem PropertyId))
findTheorem = findMaster _viewTheorems

mkStore :: FilePath -> IO Store
mkStore path = Store
  <$> openRepo path
  <*> newMVar Nothing

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

fullTheorems :: View -> [Theorem Property]
fullTheorems v = M.foldr' acc [] $ v ^. viewTheorems
  where
    props :: Map PropertyId Property
    props = v ^. viewProperties

    acc :: Theorem PropertyId -> [Theorem Property] -> [Theorem Property]
    acc t ts = case hydrateTheorem props t of
      Left  _  -> ts
      Right t' -> t' : ts

fullTraits :: View -> [(Trait Space Property, Maybe Proof)]
fullTraits v = foldr acc [] $ flattened
  where
    flattened :: [Trait SpaceId PropertyId]
    flattened = join . M.elems . M.map M.elems $ v ^. viewTraits

    acc :: Trait SpaceId PropertyId
        -> [(Trait Space Property, Maybe Proof)]
        -> [(Trait Space Property, Maybe Proof)]
    acc t ts =
      let
        sid   = traitSpace t
        pid   = traitProperty t
        ms    = M.lookup sid $ v ^. viewSpaces
        mp    = M.lookup pid $ v ^. viewProperties
        proof = M.lookup (sid, pid) $ v ^. viewProofs
      in
        case (ms, mp) of
          (Just s, Just p) -> (t { traitSpace = s, traitProperty = p }, proof) : ts
          _ -> ts
