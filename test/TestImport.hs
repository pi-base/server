module TestImport
    ( module TestImport
    , module X
    ) where

import Application           (makeFoundation, makeLogWare)
#if MIN_VERSION_classy_prelude(1, 0, 0)
import ClassyPrelude         as X hiding (delete, deleteBy, Handler)
#else
import ClassyPrelude         as X hiding (delete, deleteBy)
#endif
import Control.Monad.Trans.State as X (StateT)
import Database.Persist      as X hiding (get)
import Database.Persist.Sql  (SqlPersistM, SqlBackend, runSqlPersistMPool, rawExecute, rawSql, unSingle, connEscapeName)
import Foundation            as X
import Model                 as X
import Test.Hspec            as X
import Test.Tasty            as X (TestTree)
import Test.Tasty.Hspec      as X (testSpec)
import Yesod.Default.Config2 (useEnv, loadYamlSettings)
import Yesod.Auth            as X
import Yesod.Test            as X

import           Data.Aeson                 (ToJSON(..), Value(..), decode)
import           Data.Aeson.Encode.Pretty   (encodePretty)
import qualified Data.ByteString.Lazy       as LBS
import qualified Data.ByteString.Lazy.Char8 as LBS8
import qualified Data.Text                  as T
import qualified Data.Text.Lazy             as TL
import qualified Data.HashMap.Strict        as HM
import           Network.Wai.Test           (SResponse(..))
import qualified Test.HUnit                 as H

-- Wiping the database
import Database.Persist.Sqlite              (sqlDatabase, wrapConnection, createSqlPool)
import qualified Database.Sqlite as Sqlite
import Control.Monad.Logger                 (runLoggingT)
import Settings                             (appDatabaseConf)
import Yesod.Core                           (messageLoggerSource)

runDB :: SqlPersistM a -> YesodExample App a
runDB query = do
    pool <- fmap appConnPool getTestYesod
    liftIO $ runSqlPersistMPool query pool

buildApp :: IO (TestApp App)
buildApp = do
  settings <- loadYamlSettings
      ["config/test-settings.yml", "config/settings.yml"]
      []
      useEnv
  foundation <- makeFoundation settings
  wipeDB foundation
  logWare <- liftIO $ makeLogWare foundation
  return (foundation, logWare)

withApp :: SpecWith (TestApp App) -> Spec
withApp = before buildApp

-- This function will truncate all of the tables in your database.
-- 'withApp' calls it before each test, creating a clean environment for each
-- spec to run in.
wipeDB :: App -> IO ()
wipeDB app = do
    -- In order to wipe the database, we need to temporarily disable foreign key checks.
    -- Unfortunately, disabling FK checks in a transaction is a noop in SQLite.
    -- Normal Persistent functions will wrap your SQL in a transaction,
    -- so we create a raw SQLite connection to disable foreign keys.
    -- Foreign key checks are per-connection, so this won't effect queries outside this function.

    -- Aside: SQLite by default *does not enable foreign key checks*
    -- (disabling foreign keys is only necessary for those who specifically enable them).
    let settings = appSettings app
    sqliteConn <- rawConnection (sqlDatabase $ appDatabaseConf settings)
    disableForeignKeys sqliteConn

    let logFunc = messageLoggerSource app (appLogger app)
    pool <- runLoggingT (createSqlPool (wrapConnection sqliteConn) 1) logFunc

    flip runSqlPersistMPool pool $ do
        sqlBackend <- ask
        -- tables <- getTables -- need to control the order
        let tables = ["token", "user_branch", "branch", "user"]
        let queries = map (\t -> "DELETE FROM " ++ (connEscapeName sqlBackend $ DBName t)) tables
        forM_ queries (\q -> rawExecute q [])

rawConnection :: Text -> IO Sqlite.Connection
rawConnection t = Sqlite.open t

disableForeignKeys :: Sqlite.Connection -> IO ()
disableForeignKeys conn = Sqlite.prepare conn "PRAGMA foreign_keys = OFF;" >>= void . Sqlite.step

getTables :: MonadIO m => ReaderT SqlBackend m [Text]
getTables = do
    tables <- rawSql "SELECT name FROM sqlite_master WHERE type = 'table';" []
    return (fmap unSingle tables)

-- | Authenticate as a user. This relies on the `auth-dummy-login: true` flag
-- being set in test-settings.yaml, which enables dummy authentication in
-- Foundation.hs
authenticateAs :: Entity User -> YesodExample App ()
authenticateAs (Entity _ u) = do
    request $ do
        setMethod "POST"
        addPostParam "ident" $ userIdent u
        setUrl $ AuthR $ PluginR "dummy" []

-- | Create a user.
createUser :: Text -> Text -> YesodExample App Text
createUser ident token = do
  userId <- runDB $ insert User
      { userIdent       = ident
      , userName        = ident
      , userEmail       = ident
      , userGithubToken = ident
      }

  now <- liftIO getCurrentTime

  _ <- runDB $ insert Token
    { tokenUserId = userId
    , tokenIssuedAt = now
    , tokenExpiredAt = Nothing
    , tokenUuid = token
    }

  return ident

json :: (Value -> YesodExample App ()) -> YesodExample App ()
json action = withResponse $ \SResponse { simpleBody = body } ->
  case decode body of
    Nothing     -> liftIO $ H.assertBool "Can't parse response as JSON" False
    Just parsed -> action parsed

getResponseBody :: YesodExample site LBS.ByteString
getResponseBody = getResponse >>= return . maybe "" simpleBody

showResponse :: YesodExample a ()
showResponse = withResponse $ \SResponse { simpleBody = body } -> liftIO . putStrLn . TL.toStrict $ decodeUtf8 body

shouldHaveKey :: Value -> Text -> YesodExample App ()
shouldHaveKey (Object _map) key = liftIO $ H.assertBool msg (HM.member key _map)
  where msg = "Value does not contain key: " ++ T.unpack key
shouldHaveKey _ _ = liftIO $ H.assertBool "Value is not an object" False

traceJ :: (ToJSON a, Monad m) => a -> m ()
traceJ = traceM . LBS8.unpack . encodePretty
