{-# LANGUAGE InstanceSigs, MultiParamTypeClasses, TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Foundation where

import Import.NoFoundation
import qualified Data.Map as Map
import Database.Persist.Sql (ConnectionPool, runSqlPool)
import Text.Hamlet          (hamletFile)
import Text.Jasmine         (minifym)

import Yesod.Auth.Dummy

import Handler.Helpers (generateToken, maybeToken, requireToken)

import Yesod.Auth.OAuth2.Github
import Yesod.Default.Util   (addStaticContentExternal)
import Yesod.Core.Types     (HandlerFor(..), Logger)
import qualified Yesod.Core.Unsafe    as Unsafe
import qualified Data.Aeson           as Aeson
import qualified Data.ByteString.Lazy as LBS
import qualified Data.CaseInsensitive as CI
import qualified Data.HashMap.Strict  as HM
import qualified Data.Text            as T
import qualified Data.Text.Encoding   as TE

import Control.Monad.Catch as MC (MonadCatch(..), MonadMask(..))

import Class
import Git.Libgit2 (HasLgRepo(..))
import Data.Branch (ensureUserBranch)
import Data.Store  (Store, storeRepo)
import Services.Rollbar as Rollbar
import qualified Graph.Queries.Cache as GQ

import System.Environment (lookupEnv)
import System.IO.Unsafe   (unsafePerformIO)

-- | The foundation datatype for your application. This can be a good place to
-- keep settings and values requiring initialization before your application
-- starts running, such as database connections. Every handler will have
-- access to the data present here.
data App = App
    { appSettings    :: AppSettings
    , appStatic      :: Static -- ^ Settings for static file serving.
    , appConnPool    :: ConnectionPool -- ^ Database connection pool.
    , appQueryCache  :: GQ.Cache
    , appHttpManager :: Manager
    , appLogger      :: Logger
    , appStore       :: Store
    }

data MenuItem = MenuItem
    { menuItemLabel :: Text
    , menuItemRoute :: Route App
    , menuItemAccessCallback :: Bool
    }

data MenuTypes
    = NavbarLeft MenuItem
    | NavbarRight MenuItem

-- This is where we define all of the routes in our application. For a full
-- explanation of the syntax, please see:
-- http://www.yesodweb.com/book/routing-and-handlers
--
-- Note that this is really half the story; in Application.hs, mkYesodDispatch
-- generates the rest of the code. Please see the following documentation
-- for an explanation for this split:
-- http://www.yesodweb.com/book/scaffolding-and-the-site-template#scaffolding-and-the-site-template_foundation_and_application_modules
--
-- This function also generates the following type synonyms:
-- type Handler = HandlerT App IO
-- type Widget = WidgetT App IO ()
mkYesodData "App" $(parseRoutesFile "config/routes")

-- | A convenient synonym for creating forms.
type Form x = Html -> MForm (HandlerFor App) (FormResult x, Widget)

instance HasLgRepo Handler where
  getRepository = (storeRepo . appStore) <$> getYesod

-- http://hackage.haskell.org/package/yesod-core-1.4.37.3/docs/src/Yesod-Core-Types.html#line-455
instance MonadCatch (HandlerFor site) where
  catch (HandlerFor m) c = HandlerFor $ \r -> m r `MC.catch` \e -> unHandlerFor (c e) r
instance MonadMask (HandlerFor site) where
  mask a = HandlerFor $ \e -> MC.mask $ \u -> unHandlerFor (a $ q u) e
    where q u (HandlerFor b) = HandlerFor (u . b)
  uninterruptibleMask a =
    HandlerFor $ \e -> MC.uninterruptibleMask $ \u -> unHandlerFor (a $ q u) e
      where q u (HandlerFor b) = HandlerFor (u . b)

instance MonadStore Handler where
  getStore = appStore <$> getYesod

instance MonadDB Handler where
  db = runDB

instance MonadGraph Handler where
  getSettings = appSettings <$> getYesod
  requireUser = requireToken

data GithubUserResponse = GithubUserResponse
  { ghLogin :: Text
  , ghEmail :: Text
  }

instance Aeson.FromJSON GithubUserResponse where
  parseJSON = Aeson.withObject "UserResponse" $ \u ->
    GithubUserResponse 
      <$> u .: "login"
      <*> u .: "email"

parseGithubUserResponse :: Text -> Text -> Text -> Either String User
parseGithubUserResponse _id token response = 
  case Aeson.eitherDecode . LBS.fromStrict $ encodeUtf8 response of
    Left err -> Left err
    Right GithubUserResponse{..} -> Right $ User 
      { userIdent       = _id
      , userGithubToken = token
      , userName        = ghLogin
      , userEmail       = ghEmail
      }

createGithubUser :: User -> Handler UserId
createGithubUser user = do
  userId <- runDB $ insert user
  _ <- ensureUserBranch $ Entity userId user
  _ <- generateToken userId
  return userId

getSetting :: (AppSettings -> a) -> Handler a
getSetting f = getYesod >>= return . f . appSettings

development :: Bool
development =
#if DEVELOPMENT
    True
#else
    False
#endif

instance Rollbar.HasRollbar Handler where
  rollbar level message extra = do
    settings <- appRollbar . appSettings <$> getYesod
    request  <- getRequest
    muser    <- maybeAuth
    forkHandler ($logErrorS "Rollbar.handler" . tshow) $ 
      Rollbar.send settings $ Rollbar.Report
        { Rollbar.message = message
        , Rollbar.context = Nothing
        , Rollbar.level   = level
        , Rollbar.request = Just request
        , Rollbar.user    = muser
        , Rollbar.custom  = Just $ HM.fromList extra
        }

-- Please see the documentation for the Yesod typeclass. There are a number
-- of settings which can be configured by overriding methods here.
instance Yesod App where
    -- Controls the base of generated URLs. For more information on modifying,
    -- see: https://github.com/yesodweb/yesod/wiki/Overriding-approot
    --
    -- N.B. it looks like this has to be Static for oauth's redriect_uri to work;
    --  be sure this is set in the env _before_ here.
    approot = ApprootStatic $ unsafePerformIO $
      maybe "http://server.counterexamples.info" T.pack <$> lookupEnv "APPROOT"

    -- Store session data on the client in encrypted cookies,
    -- default session idle timeout is 120 minutes
    makeSessionBackend _ = Just <$> defaultClientSessionBackend
        120    -- timeout in minutes
        "config/client_session_key.aes"

    errorHandler err@(InternalError e) = do
      rollbar Rollbar.Error (tshow e) mempty
      defaultErrorHandler err
    errorHandler err = defaultErrorHandler err

    -- Yesod Middleware allows you to run code before and after each handler function.
    -- The defaultYesodMiddleware adds the response header "Vary: Accept, Accept-Language" and performs authorization checks.
    -- Some users may also want to add the defaultCsrfMiddleware, which:
    --   a) Sets a cookie with a CSRF token in it.
    --   b) Validates that incoming write requests include that token in either a header or POST parameter.
    -- To add it, chain it together with the defaultMiddleware: yesodMiddleware = defaultYesodMiddleware . defaultCsrfMiddleware
    -- For details, see the CSRF documentation in the Yesod.Core.Handler module of the yesod-core package.
    yesodMiddleware = defaultYesodMiddleware

    defaultLayout widget = do
        master <- getYesod
        mmsg <- getMessage

        -- We break up the default layout into two components:
        -- default-layout is the contents of the body tag, and
        -- default-layout-wrapper is the entire page. Since the final
        -- value passed to hamletToRepHtml cannot be a widget, this allows
        -- you to use normal widget features in default-layout.

        pc <- widgetToPageContent $ do
            addStylesheet $ StaticR css_bootstrap_css
            $(widgetFile "default-layout")
        withUrlRenderer $(hamletFile "templates/default-layout-wrapper.hamlet")

    -- The page to be redirected to when authentication is required.
    authRoute _ = Just $ AuthR LoginR

    -- Routes not requiring authentication.
    isAuthorized (AuthR _) _ = return Authorized
    isAuthorized ErrorR _    = return Authorized
    isAuthorized HooksR _    = return Authorized
    isAuthorized GraphR _    = return Authorized
    isAuthorized FrontendR _ = return Authorized
    isAuthorized UsersR _    = return Authorized
    isAuthorized _ False     = return Authorized
    isAuthorized _ True      = return $ Unauthorized "page is read-only"

    -- This function creates static content files in the static folder
    -- and names them based on a hash of their content. This allows
    -- expiration dates to be set far in the future without worry of
    -- users receiving stale content.
    addStaticContent ext mime content = do
        master <- getYesod
        let staticDir = appStaticDir $ appSettings master
        addStaticContentExternal
            minifym
            genFileName
            staticDir
            (StaticR . flip StaticRoute [])
            ext
            mime
            content
      where
        -- Generate a unique filename based on the content itself
        genFileName lbs = "autogen-" ++ base64md5 lbs

    shouldLogIO app _source level = return $ level >= appLogLevel (appSettings app)

    makeLogger = return . appLogger

    -- Provide proper Bootstrap styling for default displays, like
    -- error pages
    defaultMessageWidget title body = $(widgetFile "default-message-widget")

-- How to run database actions.
instance YesodPersist App where
    type YesodPersistBackend App = SqlBackend
    runDB :: SqlPersistT Handler a -> Handler a
    runDB action = do
        master <- getYesod
        runSqlPool action $ appConnPool master
instance YesodPersistRunner App where
    getDBRunner :: Handler (DBRunner App, Handler ())
    getDBRunner = defaultGetDBRunner appConnPool

instance YesodAuth App where
    type AuthId App = UserId

    -- Where to send a user after successful login
    loginDest :: App -> Route App
    loginDest _ = FrontendR
    -- Where to send a user after logout
    logoutDest :: App -> Route App
    logoutDest _ = FrontendR
    -- Override the above two destinations when a Referer: header is present
    redirectToReferer :: App -> Bool
    redirectToReferer _ = False

    authenticate :: (MonadHandler m, HandlerSite m ~ App)
                 => Creds App -> m (AuthenticationResult App)
    authenticate Creds{..} = do
      let extras = Map.fromList credsExtra
      case (Map.lookup "accessToken" extras, Map.lookup "userResponse" extras) of
        (Just token, Just response) -> liftHandler $ do
            let _id = credsPlugin <> ":" <> credsIdent
            x <- runDB $ getBy $ UniqueUser _id
            case x of
              Just (Entity uid _) -> return $ Authenticated uid
              Nothing -> case parseGithubUserResponse _id token response of
                Left   err -> return . ServerError $ "Could not parse Github data: " <> T.pack err
                Right user -> Authenticated <$> createGithubUser user
        _ -> return $ ServerError "Missing accessToken or userResponse"

    authPlugins :: App -> [AuthPlugin App]
    authPlugins app =
      let AppSettings{..} = appSettings app
          clientId = gsClientId appGithub
          clientSecret = gsClientSecret appGithub
          
          oauth = oauth2GithubScoped ["user:email,public_repo"] clientId clientSecret 
      in [oauth] ++ [authDummy | appAuthDummyLogin]

    maybeAuthId :: (MonadHandler m, App ~ HandlerSite m) => m (Maybe (AuthId App))
    maybeAuthId = (liftHandler maybeToken) >>= \case
      Just (Entity _id _) -> return $ Just _id
      Nothing             -> return Nothing

-- | Access function to determine if a user is logged in.
isAuthenticated :: Handler AuthResult
isAuthenticated = do
    muid <- maybeAuthId
    return $ case muid of
        Nothing -> Unauthorized "You must login to access this page"
        Just _ -> Authorized

instance YesodAuthPersist App

-- This instance is required to use forms. You can modify renderMessage to
-- achieve customized and internationalized form validation messages.
instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage

-- Useful when writing code that is re-usable outside of the Handler context.
-- An example is background jobs that send email.
-- This can also be useful for writing code that works across multiple Yesod applications.
instance HasHttpManager App where
    getHttpManager = appHttpManager

unsafeHandler :: App -> Handler a -> IO a
unsafeHandler = Unsafe.fakeHandlerGetLogger appLogger

-- Note: Some functionality previously present in the scaffolding has been
-- moved to documentation in the Wiki. Following are some hopefully helpful
-- links:
--
-- https://github.com/yesodweb/yesod/wiki/Sending-email
-- https://github.com/yesodweb/yesod/wiki/Serve-static-files-from-a-separate-domain
-- https://github.com/yesodweb/yesod/wiki/i18n-messages-in-the-scaffolding
