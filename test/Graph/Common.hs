{-# OPTIONS_GHC -fno-warn-orphans #-}
module Graph.Common
  ( G(..)
  , mkGraph
  , runG
  , as
  , checkout
  , checkoutUserBranch
  , head
  , login
  , logout
  , run
  , update
  -- Re-exports
  , json
  , shouldBe
  ) where

import Protolude  hiding (head)
import TestImport (App(..), TestApp, MonadCatch, MonadMask, MonadThrow, MonadUnliftIO)

import           Control.Lens              hiding ((.=))
import           Control.Monad.Logger      (MonadLogger(..))
import           Data.Aeson                (ToJSON(..), Value(..), (.=), object)
import           Data.Aeson.Lens
import           Data.Aeson.QQ             (aesonQQ)
import           Data.Char                 (toLower)
import           Data.IORef                (IORef, newIORef, readIORef, writeIORef)
import qualified Data.Text                 as T
import           Database.Persist.Sql      (ConnectionPool, Entity(..), runSqlPool)
import           Git.Libgit2               (HasLgRepo(..))
import           Language.Haskell.TH.Quote (QuasiQuoter)
import qualified Test.Tasty.Hspec          as HSpec

import           Core
import qualified Data.Branch         as Branch
import qualified Data.HashMap.Strict as HM
import           Data.Store          (storeRepo)
import qualified Graph.Root          as Root
import           Graph.Schema        (PatchInput(..))
import           Graph.Queries.Cache (Cache, mkCache)
import           Handler.Helpers     (ensureUser)
import           Settings            (AppSettings(..))

data Config = Config
  { pool     :: ConnectionPool
  , settings :: AppSettings
  , store    :: Store
  , queries  :: Cache
  , user     :: IORef (Maybe (Entity User))
  , patch    :: IORef PatchInput
  }

head :: Text
head = "3d066431b69acbb0713223203b183b0a431d5c9e"

mkGraph :: IO (TestApp App) -> IO Config
mkGraph getApp = do
  (App{..}, _) <- getApp
  Config
    <$> pure appConnPool
    <*> pure appSettings
    <*> pure appStore
    <*> (mkCache "graph" >>= either (panic "Query cache error") return)
    <*> newIORef Nothing
    <*> newIORef (PatchInput "master" head)

newtype G a = G { unG :: ReaderT Config IO a }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadReader Config
           , MonadIO
           , MonadUnliftIO
           , MonadCatch
           , MonadThrow
           , MonadMask
           )

instance MonadDB G where
  db action = asks pool >>= runSqlPool action
instance HasLgRepo G where
  getRepository = asks $ storeRepo . store
instance MonadStore G where
  getStore = asks store
instance MonadGraph G where
  requireUser = getUser
  getSettings = asks settings
instance MonadLogger G where
  monadLoggerLog _ _ _ _ = return () -- TODO: log on error+

runG :: Config -> G a -> IO a
runG config action = runReaderT (unG action) config

setUser :: Maybe (Entity User) -> G ()
setUser meu = asks user >>= \ref -> liftIO $ writeIORef ref meu

getUser :: G (Entity User)
getUser = do
  ref <- asks user
  mu  <- liftIO $ readIORef ref
  case mu of
    Nothing -> notFound "User" ("current" :: Text)
    Just eu -> return eu

setPatch :: PatchInput -> G ()
setPatch p = asks patch >>= liftIO . flip writeIORef p

as :: User -> (Entity User -> G a) -> G a
as user handler = do
  eu     <- login user
  result <- handler eu
  logout
  return result

login :: User -> G (Entity User)
login user = do
  entity <- ensureUser user
  _ <- Branch.ensureUserBranch entity
  setUser $ Just entity
  return entity

logout :: G ()
logout = setUser Nothing

checkout :: Branch -> G ()
checkout branch = do
  sha <- Branch.headSha branch
  setPatch $ PatchInput
    { branch = branchName branch
    , sha    = sha
    }

checkoutUserBranch :: G (Entity Branch)
checkoutUserBranch = do
  eb@(Entity _ branch) <- Branch.ensureUserBranch =<< getUser
  checkout branch
  return eb

run :: FilePath -> Value -> G Value
run n vars = do
  Config{..} <- ask
  let request = object
        [ "operationName" .= n
        , "variables"     .= vars
        , "query"         .= ("" :: Text)
        ]
  response <- Root.asJSON (Root.compiled settings queries) request
  return . Object $ response ^. key "data"._Object

update :: FilePath -> Value -> G Value
update n vars = do
  ref   <- asks patch
  p     <- liftIO $ readIORef ref
  data_ <- run n $ addVariable vars "patch" p
  setPatch $ p { sha = data_ ^. key (firstLower n) . key "version" . _String }
  return data_

addVariable :: ToJSON v => Value -> Text -> v -> Value
addVariable (Object m) name val = Object $ m <> HM.fromList [(name, toJSON val)]
addVariable _ _ _ = panic "Tried to add variable to non-object"

firstLower :: FilePath -> Text
firstLower (c:cs) = toLower c `T.cons` T.pack cs
firstLower [] = ""

-- Convenience re-exports

json :: QuasiQuoter
json = aesonQQ

infix 1 `shouldBe`

shouldBe :: (Eq a, Show a, MonadIO m) => a -> a -> m ()
shouldBe a b = liftIO $ HSpec.shouldBe a b