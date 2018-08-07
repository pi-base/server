{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Application
    ( getAppSettings
    , getApplicationDev
    , appMain
    , develMain
    , makeFoundation
    , makeLogWare
    -- * for DevelMain
    , getApplicationRepl
    , shutdownApp
    -- * for GHCI
    , handler
    , db
    ) where

import Control.Monad.Logger                 (liftLoc, runLoggingT)
import qualified Data.Text                  as T
import Database.Persist.Postgresql          (ConnectionPool, createPostgresqlPool, runSqlPool,
                                             pgConnStr, pgPoolSize)
import Import
import Language.Haskell.TH.Syntax           (qLocation)
import LoadEnv                              (loadEnvFrom)
import Network.Wai.Handler.Warp             (Settings, defaultSettings,
                                             defaultShouldDisplayException,
                                             runSettings, setHost,
                                             setOnException, setPort, getPort)
import Network.Wai.Middleware.Cors
import System.Environment                   (lookupEnv)
import System.Exit                          (die)
import System.Log.FastLogger                (defaultBufSize, newStdoutLoggerSet,
                                             toLogStr)

import           Data        (initializeStore)
import qualified Data.Branch as Branch
import           Data.Store  (Store)
import           Logging     (makeLogWare)

-- Import all relevant handler modules here.
-- Don't forget to add new modules to your cabal file!
import Handler.Common
import Handler.Errors
import Handler.Graph
import Handler.Home
import Handler.User

import qualified Graph.Queries.Cache as G

-- This line actually creates our YesodDispatch instance. It is the second half
-- of the call to mkYesodData which occurs in Foundation.hs. Please see the
-- comments there for more details.
mkYesodDispatch "App" resourcesApp

-- | This function allocates resources (such as a database connection pool),
-- performs initialization and returns a foundation datatype value. This is also
-- the place to put your migrate statements to have automatic database
-- migrations handled by Yesod.
makeFoundation :: AppSettings -> IO App
makeFoundation appSettings = do
    -- Some basic initializations: HTTP connection manager, logger, and static
    -- subsite.
    appHttpManager <- newManager
    appLogger <- newStdoutLoggerSet defaultBufSize >>= makeYesodLogger
    appStatic <-
        (if appMutableStatic appSettings then staticDevel else static)
        (appStaticDir appSettings)

    appQueryCache <- either (error . show) id <$> G.mkCache "graph"

    -- We need a log function to create a connection pool. We need a connection
    -- pool to create our foundation. And we need our foundation to get a
    -- logging function. To get out of this loop, we initially create a
    -- temporary foundation without a real connection pool, get a log function
    -- from there, and then create the real foundation.
    let
        mkFoundation :: ConnectionPool -> Store -> App
        mkFoundation appConnPool appStore = App {..}
        -- The App {..} syntax is an example of record wild cards. For more
        -- information, see:
        -- https://ocharles.org.uk/blog/posts/2014-12-04-record-wildcards.html
        tempFoundation :: App
        tempFoundation = mkFoundation
          (error "connPool forced in tempFoundation")
          (error "store forced in tempFoundation")
        logFunc = messageLoggerSource tempFoundation appLogger

    -- Set up the git repo
    store <- flip runLoggingT logFunc $
      initializeStore $ appRepo appSettings
    
    -- Create the database connection pool
    pool <- flip runLoggingT logFunc $ createPostgresqlPool
        (pgConnStr  $ appDatabaseConf appSettings)
        (pgPoolSize $ appDatabaseConf appSettings)

    -- Perform database migration using our application's logging settings.
    runLoggingT (runSqlPool (runMigration migrateAll) pool) logFunc

    -- Return the foundation
    return $ mkFoundation pool store

checkEnv :: IO ()
checkEnv = do
  let required = ["REPO_PATH"] -- , "GITHUB_AUTH", "GITHUB_CLIENT_ID", "GITHUB_CLIENT_SECRET"

  missing <- filterM (lookupEnv >=> return . isNothing) required
  when (length missing > 0) $ do
    putStrLn $ "Missing required environment variables:"
    forM_ missing $ \k ->
      putStrLn $ "* " <> T.pack k
    putStrLn "See config/settings.yml for more details on each key"
    putStrLn "If you do not need a particular feature, set the environment variable to a non-empty string to skip this check."
    die "Exiting"

corsPolicy :: CorsResourcePolicy
corsPolicy = simpleCorsResourcePolicy
  { corsMethods = ["GET" , "HEAD" , "POST", "OPTIONS"]
  , corsRequestHeaders = ["content-type", "authorization"]
  }

-- | Convert our foundation to a WAI Application by calling @toWaiAppPlain@ and
-- applying some additional middlewares.
makeApplication :: App -> IO Application
makeApplication foundation = do
    logWare <- makeLogWare foundation
    -- Create the WAI application and apply middlewares
    appPlain <- toWaiAppPlain foundation
    return $ cors (const $ Just corsPolicy) $ logWare $ defaultMiddlewaresNoLogging appPlain

-- | Warp settings for the given foundation value.
warpSettings :: App -> Settings
warpSettings foundation =
      setPort (appPort $ appSettings foundation)
    $ setHost (appHost $ appSettings foundation)
    $ setOnException (\_req e ->
        when (defaultShouldDisplayException e) $ messageLoggerSource
            foundation
            (appLogger foundation)
            $(qLocation >>= liftLoc)
            "yesod"
            LevelError
            (toLogStr $ "Exception from Warp: " ++ show e))
      defaultSettings

setupBranches :: Handler ()
setupBranches = do
  $(logInfo) "Setting up branches"
  void Branch.ensureBaseBranch
  void Branch.claimUserBranches

-- | For yesod devel, return the Warp settings and WAI Application.
getApplicationDev :: IO (Settings, Application)
getApplicationDev = do
    settings <- getAppSettings
    foundation <- makeFoundation settings
    unsafeHandler foundation $ setupBranches
    wsettings <- getDevSettings $ warpSettings foundation
    app <- makeApplication foundation
    return (wsettings, app)

getAppSettings :: IO AppSettings
getAppSettings = do
  loadEnvFrom ".env"
  checkEnv
  loadYamlSettings [configSettingsYml] [] useEnv

-- | main function for use by yesod devel
develMain :: IO ()
develMain = develMainHelper getApplicationDev

-- | The @main@ function for an executable running this site.
appMain :: IO ()
appMain = do
    -- Get the settings from all relevant sources
    settings <- loadYamlSettingsArgs
        -- fall back to compile-time values, set to [] to require values at runtime
        [configSettingsYmlValue]

        -- allow environment variables to override
        useEnv

    -- Generate the foundation from the settings
    foundation <- makeFoundation settings

    -- Generate a WAI Application from the foundation
    app <- makeApplication foundation

    -- Run boot-time "handlers"
    _ <- unsafeHandler foundation $ do
      setupBranches
      $(logInfo) $ "App starting on port " <> tshow (appPort settings)

    -- Run the application with Warp
    runSettings (warpSettings foundation) app


--------------------------------------------------------------
-- Functions for DevelMain.hs (a way to run the app from GHCi)
--------------------------------------------------------------
getApplicationRepl :: IO (Int, App, Application)
getApplicationRepl = do
    settings <- getAppSettings
    foundation <- makeFoundation settings
    wsettings <- getDevSettings $ warpSettings foundation
    app1 <- makeApplication foundation
    return (getPort wsettings, foundation, app1)

shutdownApp :: App -> IO ()
shutdownApp _ = return ()


---------------------------------------------
-- Functions for use in development with GHCi
---------------------------------------------

-- | Run a handler
handler :: Handler a -> IO a
handler h = getAppSettings >>= makeFoundation >>= flip unsafeHandler h

-- | Run DB queries
db :: ReaderT SqlBackend (HandlerFor App) a -> IO a
db = handler . runDB
