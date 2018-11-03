module Config
  ( server
  , test
  ) where

import Core

import qualified Data.ByteString.Char8       as BS
import qualified Data.Text                   as T
import           Database.Persist.Postgresql (PostgresConf(..))
import           System.Environment          (lookupEnv)

import qualified Build
import qualified Logger
import qualified Data.Store       as Store
import qualified Graph.Types      as Graph
import qualified Services.Rollbar as Rollbar
import qualified Services.Github  as Github

server :: IO Settings
server = Settings
  <$> def "FRONTEND_URL" "https://viewer.counterxamples.info"
  <*> def "VERBOSITY"    Logger.LevelInfo
  <*> def "PORT"         3141
  <*> do
      connString <- mkConnStr
        <$> def "DATABASE_NAME" "pi_base_development"
        <*> def "DATABASE_HOST" "localhost"
        <*> opt "DATABASE_USER"
        <*> opt "DATABASE_PASSWORD"
      dbPool <- def "DATABASE_POOL_SIZE" 10
      return $ PostgresConf connString dbPool
  <*> ( Github.Settings
      <$> req "GITHUB_TOKEN"
      <*> def "GITHUB_OWNER" "pi-base"
      <*> def "GITHUB_REPO"  "data"
      <*> req "GITHUB_CLIENT_ID"
      <*> req "GITHUB_CLIENT_SECRET"
      <*> req "GITHUB_CALLBACK_URI"
      <*> req "GITHUB_WEBHOOK_SECRET"
      )
  <*> ( Graph.Settings
      <$> opt "GRAPH_PATH"
      )
  <*> ( Store.Settings
      <$> def "REPO_PATH"        "/app/data/repo.git"
      <*> def "REPO_BASE_BRANCH" "master"
      <*> def "REPO_AUTO_PUSH"   False
      <*> def "REPO_UPSTREAM"    "git@github.com:pi-base/data.git"
      )
  <*> ( Rollbar.Settings
      <$> opt "ROLLBAR_TOKEN"
      <*> def "ROLLBAR_ENVIRONMENT" "production"
      <*> pure (maybe "" Build.sha Build.info)
      )

test :: IO Settings
test = Settings
  <$> pure "https://viewer.example.com"
  <*> pure LevelError
  <*> pure 3999
  <*> do
      connString <- mkConnStr
        <$> pure "pi_base_test"
        <*> def "DATABASE_HOST" "localhost"
        <*> opt "DATABASE_USER"
        <*> opt "DATABASE_PASSWORD"
      dbPool <- def "DATABASE_POOL_SIZE" 10
      return $ PostgresConf connString dbPool
  <*> ( Github.Settings
      <$> pure "xxx"
      <*> def "GITHUB_OWNER" "pi-base"
      <*> def "GITHUB_REPO"  "data"
      <*> pure "xxx"
      <*> pure "xxx"
      <*> pure "xxx"
      <*> pure "xxx"
      )
  <*> ( Graph.Settings
      <$> pure (Just "graph")
      )
  <*> ( Store.Settings
      <$> pure "tmp/repo.git"
      <*> pure "test"
      <*> pure False
      <*> pure "tmp/upstream.git"
      )
  <*> ( Rollbar.Settings
      <$> pure Nothing
      <*> pure "test"
      <*> pure (maybe "" Build.sha Build.info)
      )

opt :: FromEnv a => String -> IO (Maybe a)
opt var = lookupEnv var >>= \case
  Just val -> return $ fromEnv val
  _ -> return Nothing

def :: FromEnv a => String -> a -> IO a
def var val = opt var >>= return . fromMaybe val

req :: FromEnv a => String -> IO a
req var = opt var >>= \case
  Just val -> return val
  Nothing  -> die $ "Missing required ENV var: " <> T.pack var

class FromEnv a where
  fromEnv :: String -> Maybe a

instance FromEnv String where
  fromEnv = Just

instance FromEnv ByteString where
  fromEnv = Just . BS.pack

instance FromEnv Text where
  fromEnv = Just . T.pack

instance FromEnv Int where
  fromEnv = readMaybe

instance FromEnv LogLevel where
  fromEnv l = case Logger.parseLogLevel l of
    Right lvl -> Just lvl
    Left    _ -> Nothing

instance FromEnv Bool where
  fromEnv "true"  = Just True
  fromEnv "yes"   = Just True
  fromEnv "1"     = Just True
  fromEnv "false" = Just False
  fromEnv "no"    = Just False
  fromEnv "0"     = Just False
  fromEnv _       = Nothing

mkConnStr :: String -> String -> Maybe String -> Maybe String -> ByteString
mkConnStr name host muser mpass = BS.pack $ intercalate " "
  [ "host=" <> host
  , "dbname=" <> name
  , maybe "" ("user=" <>) muser
  , maybe "" ("pass=" <>) mpass
  ]
