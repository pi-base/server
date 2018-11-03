{-# LANGUAGE TemplateHaskell #-}
module Config.Boot
  ( boot
  ) where

import Server.Import

import Database.Persist.Postgresql (createPostgresqlPool, pgConnStr, pgPoolSize, runMigration, runSqlPool)
import Data.Time                   (getCurrentTime)

import qualified Build
import qualified Data.Branch as Branch
import qualified Data.Store  as Store
import qualified Graph
import           Logger
import           Model       (migrateAll)

boot :: Settings -> IO Env
boot settings = do
  foundation <- makeFoundation settings

  now <- getCurrentTime
  let env = Env
          { _envBootTime   = now
          , _envBuildInfo  = Build.info
          , _envFoundation = foundation
          , _envSettings   = settings
          }

  runApp env $ setupBranches

  return env

setupBranches :: App m => m ()
setupBranches = do
  $(logInfo) "Setting up branches"
  void Branch.ensureBaseBranch
  void Branch.claimUserBranches

initCache :: FilePath -> IO (Maybe Graph.QueryCache)
initCache path = Graph.mkCache path >>= either (die . show) (return . Just)

makeFoundation :: Settings -> IO Foundation
makeFoundation settings = do
  logger <- Logger.build settings

  Logger.withLogging logger $ do
    let dbConf = settings ^. databaseSettings
    pool <- createPostgresqlPool (pgConnStr dbConf) (pgPoolSize dbConf)

    runSqlPool (runMigration migrateAll) pool

    store <- Store.build (settings ^. repoSettings) >>= either (lift . die) return

    cache <- maybe (return Nothing) (lift . initCache) $ settings ^. graphSettings . Graph.queryStore

    return Foundation
      { _appConnPool = pool
      , _appLogger   = logger
      , _appQueries  = cache
      , _appStore    = store
      }
