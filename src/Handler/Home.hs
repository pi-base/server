module Handler.Home where

import Import

import Data.Store                  (getStoreBaseVersion)
import Database.Persist.Postgresql (PostgresConf(..))
import Handler.Helpers             (generateToken)
import Services.Github             (checkPullRequest, webhookHandler)
import Core                        (View(..), Version(..))

getHomeR :: Handler Value
getHomeR = do
  version  <- getStoreBaseVersion
  build    <- getSetting appBuild
  render   <- getUrlRender
#if DEVELOPMENT
  settings <- appSettings <$> getYesod
#endif
  return $ object 
    [ "version" .= version
    , "build"   .= build
    , "root"    .= render HomeR
#if DEVELOPMENT
    , "settings" .= debugSettings settings
#endif
    ]

debugSettings :: AppSettings -> Value
debugSettings AppSettings{..} = object
  [ "database" .= object
    [ "conn" .= (tshow $ pgConnStr appDatabaseConf)
    , "pool" .= pgPoolSize appDatabaseConf
    ]
  , "repo" .= object
    [ "path"     .= rsPath appRepo
    , "upstream" .= rsUpstream appRepo
    ]
  ]

postHooksR :: Handler Value
postHooksR = do
  pullRequest <- webhookHandler
  result      <- checkPullRequest pullRequest
  let version = (maybe "??" unVersion . _viewVersion) <$> result
  either throwIO returnJson version

activeToken :: UserId -> Handler Token
activeToken userId = do
  token <- runDB $ selectFirst [TokenUserId ==. userId] [] -- TODO: not expired
  case token of
    Just (Entity _ t) -> return t
    _ -> generateToken userId

getFrontendR :: Handler ()
getFrontendR = do
  frontendUrl <- getSetting appFrontendUrl
  defaultMaybeAuthId >>= \case
    Nothing -> redirect frontendUrl
    Just _id -> (runDB $ get _id) >>= \case
      Nothing -> redirect frontendUrl
      Just _ -> do
        Token{..} <- activeToken _id
        redirect $ frontendUrl <> "/login/" <> tokenUuid
