module Data
  ( mkStore
  , storeMaster
  , parseViewer
  , viewerAtRef
  , fetchPullRequest
  , updatedPages
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
import qualified Data.UUID       as UUID
import qualified Data.UUID.V4    as UUID
import           System.Process  (callCommand)

import qualified Data.Parse as P
import qualified Logic      as L
import qualified View       as V

import qualified Page.Parser
import qualified Page.Property
import qualified Page.Space
import qualified Page.Theorem
import qualified Page.Trait

import Core     hiding (assert)
import Data.Git (openRepo, useRepo, writeContents)
import Util     (fetch)

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

updateSpace :: MonadStore m
            => User -> Space -> Text -> m (Maybe Space)
updateSpace user space description = useRepo $ do
    let updated = space { spaceDescription = description }
    writeContents user ("Updated " <> spaceName space)
      [Page.Parser.write $ Page.Space.write space]
    return $ Just updated

makeId :: MonadIO m => (Text -> a) -> Text -> m a
makeId constructor prefix = do
  uuid <- liftIO UUID.nextRandom
  return . constructor $ prefix <> UUID.toText uuid

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
assertTrait user sid pid value description = assert user
  (P.viewSpace sid)
  (\v -> buildTrait v sid pid value description)
  L.assertTrait
  (\trait -> "Add " <> traitName trait)

assertTheorem :: MonadStore  m
              => User -> Formula PropertyId -> Formula PropertyId -> Text -> m (Either [Error] View)
assertTheorem user ant con desc = assert user P.viewer
  (\v -> buildTheorem v ant con desc)
  L.assertTheorem
  (\theorem -> "Add " <> theoremName theorem)

assert :: MonadStore m
       => User
       -> (Committish -> m (Either [Error] View))
       -> (View -> m a)
       -> (a -> L.Logic ())
       -> (a -> Text)
       -> m (Either [Error] View)
assert user getView build logic message = getView (userBranchRef user) >>= \case
  Left errs -> return $ Left errs
  Right v -> do
    obj <- build v
    case L.updates v $ logic obj of
      Left errs -> return $ Left [LogicError errs]
      Right updates -> persistUpdates updates user (message obj)

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

updatedPages :: View -> [(TreeFilePath, Text)]
updatedPages v = map (Page.Parser.write . Page.Theorem.write) (V.theorems v)
              <> map (Page.Parser.write . Page.Trait.write)   (V.traits   v)

persistUpdates :: MonadStore m => View -> User -> Text -> m (Either [Error] View)
persistUpdates v user message = useRepo $ do
  writeContents user message (updatedPages v) >>= \case
    Left      msg -> return . Left  $ [PersistError msg]
    Right version -> return . Right $ v { _viewVersion = Just version }

findMaster :: (Ord k, MonadStore m)
           => (View -> Map k a)
           -> k
           -> m (Maybe a)
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

