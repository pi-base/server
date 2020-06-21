{-# LANGUAGE TemplateHaskell #-}
module Persist.DB
  ( DB
  , Config(..)
  , Disallowed(..)
  , Env
  , allBranches
  , checkBranchAccess
  , connection
  , createToken
  , createUser
  , disallowed
  , ensureBranch
  , findBranch
  , grantBranchAccess
  , initialize
  -- , migrate
  , query
  , runIO
  , userForToken
  ) where

import Core

import qualified Data.Branch  as Branch
import           Data.String  (fromString)
import qualified Data.UUID    as UUID
import qualified Data.UUID.V4 as UUID
import           Polysemy     (embed)

import qualified Data.User                   as User
import qualified Data.UserBranch             as UserBranch
import           Database.Beam
import           Database.Beam.Postgres
import qualified Database.Beam.Postgres.Full as PG
import qualified Database.PostgreSQL.Simple  as SQL

data Tables f = Tables
  { _branches     :: f (TableEntity Branch')
  , _users        :: f (TableEntity User')
  , _userBranches :: f (TableEntity UserBranch')
  } deriving (Generic, Database Postgres)

makeLenses ''Tables

db :: DatabaseSettings Postgres Tables
db = defaultDbSettings
  `withDbModification` dbModification { _branches     = setEntityName "branch" }
  `withDbModification` dbModification { _users        = setEntityName "user" }
  `withDbModification` dbModification { _userBranches = setEntityName "user_branch" }

data DB m a where
  Query :: Pg a -> DB m a

makeSem ''DB

data Disallowed = Disallowed
  deriving (Show, Eq)

instance Exception Disallowed

data Config = Config
  { connString :: ByteString
  } deriving (Generic, Show, Eq)

data Env = Env
  { connection :: Connection
  }

initialize :: Config -> IO Env
initialize c = Env
  <$> connectPostgreSQL (connString c)

runIO :: Member (Embed IO) r
      => Env -> Sem (DB ': r) a -> Sem r a
runIO env = interpret \case
  Query q -> embed $ runBeamPostgres (connection env) q

disallowed :: Member (Embed IO) r
           => Sem (DB ': r) a -> Sem r a
disallowed = interpret \case
  Query _ -> throwIO Disallowed

-- migrate :: Member DB r => Sem r ()
-- migrate = query $ runMigration migrateAll

allBranches :: Member DB r => Sem r [Branch]
allBranches = query $ runSelectReturningList
  $ select
  $ all_ $ db ^. branches

ensureBranch :: Member DB r => Branch -> Sem r ()
ensureBranch branch = query $ runInsert
  $ PG.insert (db ^. branches) (insertValues [branch])
  $ PG.onConflict PG.anyConflict PG.onConflictDoNothing

findBranch :: Member DB r => Branch.Name -> Sem r (Maybe Branch)
findBranch name = query $ runSelectReturningOne -- TODO: does this need a limit_ 1?
  $ select
  $ filter_ (\row -> row ^. Branch.name ==. val_ name)
  $ all_ (db ^. branches)

createUser :: Member DB r => User -> Sem r ()
createUser user = query $ runInsert
  $ PG.insert (db ^. users) (insertValues [user])
  $ PG.onConflict PG.anyConflict PG.onConflictDoNothing

checkBranchAccess :: Member DB r => User -> Branch.Name -> Sem r Access
checkBranchAccess user branch = query $ do
  found <- runSelectReturningOne $ select $ do
    ub <- all_ (db ^. userBranches)
    u  <- oneToMany_ (db ^. users) (\u -> primaryKey u) ub
    -- b  <- all_ (db ^. branch)

    -- guard_ ((ub ^. UserBranch.userId) `references_` u)
    -- guard_ $ ub ^. UserBranch.branchId `references_` b

    -- filter_ $ \r -> r ^. User.name ==. user ^. User.name

    return ub
  case found of
    Just ub -> return $ ub ^. UserBranch.access
    _ -> return None

-- checkBranchAccess user name = query $ do
--   -- TODO: join
--   getBy (UniqueBranchName name) >>= \case
--     Nothing -> return None
--     Just (Entity branchId _) -> do
--       userId <- putUser user
--       getBy (UniqueUserBranch userId branchId) >>= \case
--         Nothing -> return None
--         Just (Entity _ userBranch) -> return $ userBranchAccess userBranch

grantBranchAccess :: Member DB r => User -> Branch.Name -> Access -> Sem r ()
grantBranchAccess = error "grantBranchAccess"
-- grantBranchAccess user name level = query $
--   getBy (UniqueBranchName name) >>= \case
--     Nothing -> return () -- TODO: should this create?
--     Just (Entity branchId _) -> do
--       -- TODO: we probably shouldn't be implicitly creating users like this
--       userId <- putUser user
--       -- TODO: needs to update if exists
--       void $ insertBy $ UserBranch userId branchId level

createToken :: Members '[DB, Embed IO] r => User -> Sem r Token
createToken = error "createToken"
-- createToken user = do
--   id    <- query $ either entityKey identity <$> insertBy user
--   now   <- embed $ getCurrentTime
--   uuid  <- embed $ UUID.toText <$> UUID.nextRandom
--   let token = Token id now Nothing uuid
--   either entityVal (const token) <$> query (insertBy token)

userForToken :: Member DB r => Text -> Sem r (Maybe User)
userForToken = error "userForToken"
-- userForToken key = query $ do
--   -- TODO: filter out expired tokens
--   -- TODO: join
--   found <- getBy $ UniqueToken key
--   case found of
--     Nothing -> return Nothing
--     Just (Entity _ token) -> do
--       get $ tokenUserId token

-- putUser :: ( PersistUniqueWrite backend
--            , MonadIO m
--            , PersistEntityBackend User ~ BaseBackend backend
--            )
--            => User
--            -> ReaderT backend m UserId
-- putUser user = either entityKey identity <$> insertBy user

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
