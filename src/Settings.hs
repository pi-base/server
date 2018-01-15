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
import Database.Persist.Sqlite     (SqliteConf)
import Development.GitRev          (gitHash)
import Language.Haskell.TH.Syntax  (Exp, Name, Q)
import Network.Wai.Handler.Warp    (HostPreference)
import Yesod.Default.Config2       (applyEnvValue, configSettingsYml)
import Yesod.Default.Util          (WidgetFileSettings, widgetFileNoReload,
                                    widgetFileReload)

import qualified GitHub
import qualified Rollbar

-- | Runtime settings to configure this application. These settings can be
-- loaded from various sources: defaults, environment variables, config files,
-- theoretically even a database.
data AppSettings = AppSettings
    { appStaticDir              :: String
    -- ^ Directory from which to serve static files.
    , appDatabaseConf           :: SqliteConf
    -- ^ Configuration settings for accessing the database.
    , appRoot                   :: Maybe Text
    -- ^ Base for all generated URLs. If @Nothing@, determined
    -- from the request headers.
    , appHost                   :: HostPreference
    -- ^ Host/interface the server should bind to.
    , appPort                   :: Int
    -- ^ Port to listen on
    , appIpFromHeader           :: Bool
    -- ^ Get the IP address from the header when logging. Useful when sitting
    -- behind a reverse proxy.

    , appReloadTemplates        :: Bool
    -- ^ Use the reload version of templates
    , appMutableStatic          :: Bool
    -- ^ Assume that files in the static dir may change after compilation
    , appSkipCombining          :: Bool
    -- ^ Perform no stylesheet/script combining

    -- Example app-specific configuration values.
    , appCopyright              :: Text
    -- ^ Copyright text to appear in the footer of the page
    , appAnalytics              :: Maybe Text
    -- ^ Google Analytics code

    , appAuthDummyLogin         :: Bool
    -- ^ Indicate if auth dummy login should be enabled

    , appRollbar                :: Rollbar.Settings

    , appBuild                  :: Text
    , appRepoPath               :: FilePath
    , appFrontendUrl            :: Text
    , appLogLevel               :: LogLevel

    , appGitHubToken            :: GitHub.Auth
    , appGitHubOwner            :: GitHub.Name GitHub.Owner
    , appGitHubRepo             :: GitHub.Name GitHub.Repo
    , appGitHubWebhookSecret    :: Text
    , appGitHubClientId         :: Text
    , appGitHubClientSecret     :: Text
    , appDefaultBranch          :: Text
    , appTestMode               :: Bool
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
        appRoot                   <- o .:? "approot"
        appHost                   <- fromString <$> o .: "host"
        appPort                   <- o .: "port"
        appIpFromHeader           <- o .: "ip-from-header"

        appReloadTemplates        <- o .:? "reload-templates" .!= defaultDev
        appMutableStatic          <- o .:? "mutable-static"   .!= defaultDev
        appSkipCombining          <- o .:? "skip-combining"   .!= defaultDev

        appCopyright              <- o .:  "copyright"
        appAnalytics              <- o .:? "analytics"

        appAuthDummyLogin         <- o .:? "auth-dummy-login" .!= defaultDev

        appRepoPath    <- o .:  "repo-path"
        appFrontendUrl <- o .:? "frontend-url" .!= "http://localhost:3000"

        appGitHubOwner <- o .: "github-owner"
        appGitHubRepo  <- o .: "github-repo"

        appGitHubWebhookSecret <- o .: "github-webhook-secret"
        appGitHubToken         <- (GitHub.OAuth . fromString) <$> o .: "github-auth"

        appLogLevel <- logLevel <$> o .: "log-level"

        appGitHubClientId     <- o .: "github-client-id"
        appGitHubClientSecret <- o .: "github-client-secret"

        appDefaultBranch  <- o .: "default-branch"

        appTestMode <- o .: "test-mode"

        let appBuild = $(gitHash)

        rollbarToken <- o .:? "rollbar-token"
        rollbarEnv   <- o .: "rollbar-environment"
        rollbarHost  <- o .: "rollbar-host"
        let appRollbar = Rollbar.Settings
              { Rollbar.environment = Rollbar.Environment rollbarEnv
              , Rollbar.token = Rollbar.ApiToken $ maybe "" id rollbarToken
              , Rollbar.hostName = rollbarHost
              , Rollbar.reportErrors = isJust rollbarToken
              }

        return AppSettings {..}

logLevel :: String -> LogLevel
logLevel "error" = LevelError
logLevel "warn" = LevelWarn
logLevel "info" = LevelInfo
logLevel "debug" = LevelDebug
logLevel other = LevelOther $ T.pack other

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
