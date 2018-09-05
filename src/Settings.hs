{-# LANGUAGE TemplateHaskell #-}
-- | Settings are centralized, as much as possible, into this file. This
-- includes database connection settings, static file locations, etc.
-- In addition, you can configure a number of different aspects of Yesod
-- by overriding methods in the Yesod typeclass. That instance is
-- declared in the Foundation.hs file.
module Settings where

import ClassyPrelude.Yesod
import qualified Control.Exception as Exception
import Data.Aeson                  (Result (..), fromJSON, withObject, (.!=),
                                    (.:?))
import Data.FileEmbed              (embedFile)
import qualified Data.Text         as T
import Data.Yaml                   (decodeEither')
import Database.Persist.Postgresql (PostgresConf)
import Language.Haskell.TH.Syntax  (Exp, Name, Q)
import Network.Wai.Handler.Warp    (HostPreference)
import Yesod.Default.Config2       (applyEnvValue, configSettingsYml)
import Yesod.Default.Util          (WidgetFileSettings, widgetFileNoReload,
                                    widgetFileReload)

import qualified Services.Rollbar.Types as Rollbar
import Util.TH (buildEnv)

data RepoSettings = RepoSettings
  { rsPath          :: FilePath
  , rsDefaultBranch :: Text
  , rsAutoPush      :: Bool
  , rsUpstream      :: Text
  } deriving (Show, Eq)

data GithubSettings = GithubSettings
  { gsToken         :: Text
  , gsOwner         :: Text
  , gsRepo          :: Text
  , gsClientId      :: Text
  , gsClientSecret  :: Text
  , gsWebhookSecret :: Text
  } deriving (Show, Eq)

data CISettings = CISettings
  { ciBuild :: Text
  , ciSha :: Text
  } deriving (Show, Eq)

ciSettings :: CISettings
ciSettings = CISettings
  (maybe "" T.pack $(buildEnv "CIRCLE_BUILD_NUM"))
  (maybe "" T.pack $(buildEnv "CIRCLE_SHA1"))

-- | Runtime settings to configure this application. These settings can be
-- loaded from various sources: defaults, environment variables, config files,
-- theoretically even a database.
data AppSettings = AppSettings
    { appStaticDir           :: String
    , appDatabaseConf        :: PostgresConf
    , appHost                :: HostPreference
    , appPort                :: Int
    , appIpFromHeader        :: Bool

    , appReloadTemplates     :: Bool
    , appMutableStatic       :: Bool
    , appSkipCombining       :: Bool

    , appCopyright           :: Text
    , appAnalytics           :: Maybe Text

    , appAuthDummyLogin      :: Bool

    , appFrontendUrl         :: Text
    , appLogLevel            :: LogLevel
    , appTestMode            :: Bool
    , appErrorToken          :: Text
    , appCompileQueries      :: Bool

    , appRepo                :: RepoSettings
    , appRollbar             :: Rollbar.Settings
    , appGithub              :: GithubSettings
    }

instance FromJSON AppSettings where
    parseJSON = withObject "AppSettings" $ \o -> do
        let defaultDev =
#if DEVELOPMENT
                True
#else
                False
#endif
        appStaticDir              <- o .: "static-dir"
        appDatabaseConf           <- o .: "database"
        appHost                   <- fromString <$> o .: "host"
        appPort                   <- o .: "port"
        appIpFromHeader           <- o .: "ip-from-header"

        appReloadTemplates        <- o .:? "reload-templates" .!= defaultDev
        appMutableStatic          <- o .:? "mutable-static"   .!= defaultDev
        appSkipCombining          <- o .:? "skip-combining"   .!= defaultDev

        appCopyright              <- o .:  "copyright"
        appAnalytics              <- o .:? "analytics"

        appAuthDummyLogin         <- o .:? "auth-dummy-login" .!= defaultDev

        appFrontendUrl    <- o .: "frontend-url"
        appLogLevel       <- logLevel <$> o .: "log-level"
        appTestMode       <- o .: "test-mode"
        appErrorToken     <- o .: "error-token"
        appCompileQueries <- o .: "compile-queries"

        repo <- o .: "repo"
        appRepo <- RepoSettings
          <$> repo .: "path" .!= error "Repo path should be set in ENV at run time"
          <*> repo .: "default-branch"
          <*> repo .: "auto-push"
          <*> repo .: "upstream"

        rollbar      <- o .: "rollbar"
        rollbarToken <- rollbar .:? "token"
        rollbarEnv   <- rollbar .: "environment"
        rollbarHost  <- rollbar .: "host"
        let appRollbar = Rollbar.Settings
              { Rollbar.token       = rollbarToken
              , Rollbar.environment = rollbarEnv
              , Rollbar.hostname    = rollbarHost
              , Rollbar.active      = isJust rollbarToken
              , Rollbar.build       = ciBuild ciSettings
              }

        gh <- o .: "github"
        appGithub <- GithubSettings
          <$> gh .: "token"
          <*> gh .: "owner"
          <*> gh .: "repo"
          <*> gh .: "client-id"
          <*> gh .: "client-secret"
          <*> gh .: "webhook-secret"

        return AppSettings {..}

logLevel :: String -> LogLevel
logLevel "error" = LevelError
logLevel "warn"  = LevelWarn
logLevel "info"  = LevelInfo
logLevel "debug" = LevelDebug
logLevel other   = LevelOther $ T.pack other

-- | Settings for 'widgetFile', such as which template languages to support and
-- default Hamlet settings.
--
-- For more information on modifying behavior, see:
--
-- https://github.com/yesodweb/yesod/wiki/Overriding-widgetFile
widgetFileSettings :: WidgetFileSettings
widgetFileSettings = def

-- | How static files should be combined.
combineSettings :: CombineSettings
combineSettings = def

-- The rest of this file contains settings which rarely need changing by a
-- user.

widgetFile :: String -> Q Exp
widgetFile = (if appReloadTemplates compileTimeAppSettings
                then widgetFileReload
                else widgetFileNoReload)
              widgetFileSettings

-- | Raw bytes at compile time of @config/settings.yml@
configSettingsYmlBS :: ByteString
configSettingsYmlBS = $(embedFile configSettingsYml)

-- | @config/settings.yml@, parsed to a @Value@.
configSettingsYmlValue :: Value
configSettingsYmlValue = either Exception.throw id
                       $ decodeEither' configSettingsYmlBS

-- | A version of @AppSettings@ parsed at compile time from @config/settings.yml@.
compileTimeAppSettings :: AppSettings
compileTimeAppSettings =
    case fromJSON $ applyEnvValue False mempty configSettingsYmlValue of
        Error e -> error e
        Success settings -> settings

-- The following two functions can be used to combine multiple CSS or JS files
-- at compile time to decrease the number of http requests.
-- Sample usage (inside a Widget):
--
-- > $(combineStylesheets 'StaticR [style1_css, style2_css])

combineStylesheets :: Name -> [Route Static] -> Q Exp
combineStylesheets = combineStylesheets'
    (appSkipCombining compileTimeAppSettings)
    combineSettings

combineScripts :: Name -> [Route Static] -> Q Exp
combineScripts = combineScripts'
    (appSkipCombining compileTimeAppSettings)
    combineSettings
