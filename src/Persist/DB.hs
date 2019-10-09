{-# LANGUAGE TemplateHaskell #-}
module Persist.DB
  ( DB
  , Config(..)
  , Disallowed(..)
  , Env
  , allBranches
  , checkBranchAccess
  , createToken
  , createUser
  , disallowed
  , ensureBranch
  , findBranch
  , grantBranchAccess
  , initialize
  , migrate
  , query
  , runIO
  , userForToken
  ) where

import Core

import           Control.Monad.Logger        (runNoLoggingT)
import qualified Data.Branch                 as Branch
import qualified Data.UUID                   as UUID
import qualified Data.UUID.V4                as UUID
import           Database.Persist
import           Database.Persist.Postgresql (createPostgresqlPool, runMigration, runSqlPool)
import           Database.Persist.Sql        (ConnectionPool, SqlBackend)
import           Polysemy                    (embed)

data DB m a where
  Query :: ReaderT SqlBackend IO a -> DB m a

data Disallowed = Disallowed
  deriving (Show, Eq)

instance Exception Disallowed

data Config = Config
  { connectionString   :: ByteString
  , connectionPoolSize :: Int
  } deriving (Generic, Show, Eq)

data Env = Env
  { connectionPool :: ConnectionPool
  }

makeSem ''DB

runIO :: Member (Embed IO) r
      => Env -> Sem (DB ': r) a -> Sem r a
runIO env = interpret \case
  Query q -> embed $ runSqlPool q (connectionPool env)

disallowed :: Member (Embed IO) r
           => Sem (DB ': r) a -> Sem r a
disallowed = interpret \case
  Query _ -> throwIO Disallowed

migrate :: Member DB r => Sem r ()
migrate = query $ runMigration migrateAll

allBranches :: Member DB r => Sem r [Branch]
allBranches = query $
  map entityVal <$> selectList [] []

ensureBranch :: Member DB r => Branch -> Sem r ()
ensureBranch = void . query . insertBy

findBranch :: Member DB r => Branch.Name -> Sem r (Maybe Branch)
findBranch name = fmap entityVal <$> (query $ getBy $ UniqueBranchName name)

createUser :: Member DB r => User -> Sem r ()
createUser user = query $ void $ putUser user

checkBranchAccess :: Member DB r => User -> Branch.Name -> Sem r Access
checkBranchAccess user name = query $ do
  -- TODO: join
  getBy (UniqueBranchName name) >>= \case
    Nothing -> return None
    Just (Entity branchId _) -> do
      userId <- putUser user
      getBy (UniqueUserBranch userId branchId) >>= \case
        Nothing -> return None
        Just (Entity _ userBranch) -> return $ userBranchAccess userBranch

grantBranchAccess :: Member DB r => User -> Branch.Name -> Access -> Sem r ()
grantBranchAccess user name level = query $
  getBy (UniqueBranchName name) >>= \case
    Nothing -> return () -- TODO: should this create?
    Just (Entity branchId _) -> do
      -- TODO: we probably shouldn't be implicitly creating users like this
      userId <- putUser user
      -- TODO: needs to update if exists
      void $ insertBy $ UserBranch userId branchId level

createToken :: Members '[DB, Embed IO] r
            => User
            -> Sem r Token
createToken user = do
  id    <- query $ either entityKey identity <$> insertBy user
  now   <- embed $ getCurrentTime
  uuid  <- embed $ UUID.toText <$> UUID.nextRandom
  let token = Token id now Nothing uuid
  either entityVal (const token) <$> query (insertBy token)

userForToken :: Member DB r => Text -> Sem r (Maybe User)
userForToken key = query $ do
  -- TODO: filter out expired tokens
  -- TODO: join
  found <- getBy $ UniqueToken key
  case found of
    Nothing -> return Nothing
    Just (Entity _ token) -> do
      get $ tokenUserId token

putUser :: ( PersistUniqueWrite backend
           , MonadIO m
           , PersistEntityBackend User ~ BaseBackend backend
           )
           => User
           -> ReaderT backend m UserId
putUser user = either entityKey identity <$> insertBy user

initialize :: MonadIO m => Config -> m Env
initialize Config{..} = liftIO $ do
  connectionPool <- runNoLoggingT $ createPostgresqlPool connectionString connectionPoolSize
  return Env{..}

-- withAccess :: User -> DB [Branch]
-- withAccess user = db $ do
--   userId <- ensureUser' user
--   -- TODO: join
--   userBranches <- selectList [UserBranchUserId ==. userId] []
--   let branchIds = map (userBranchBranchId . entityVal) userBranches
--   map entityVal <$> selectList [BranchId <-. branchIds] []

-- activeToken :: UserId -> DB (Maybe Token)
-- activeToken id = do
--   -- TODO: filter out expired tokens
--   found <- db $ selectFirst [TokenUserId ==. id] [Desc TokenIssuedAt]
--   return $ case found of
--     Just (Entity _ token) -> Just token
--     _ -> Nothing

-- ensureUser :: User -> DB UserId
-- ensureUser = db . putUser
