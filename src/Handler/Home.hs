module Handler.Home where

import Import

import Data.Store                  (getStoreBaseVersion)
import Database.Persist.Postgresql (PostgresConf(..))
import Handler.Helpers             (generateToken)
import Services.Github             (checkPullRequest, webhookHandler)
import Core                        (View(..), Version(..))

#if DEVELOPMENT
#else
import Settings (ciSettings)
#endif

getHomeR :: Handler Value
getHomeR = do
  version  <- getStoreBaseVersion
  render   <- getUrlRender
  yesod    <- getYesod
  return $ object
    [ "HEAD"       .= version
    , "start"      .= appStart yesod
    , "urls" .= object
      [ "root"   .= render HomeR
      , "graph"  .= render GraphR
      , "schema" .= render SchemaR
      ]
#if DEVELOPMENT
    , "settings" .= debugSettings (appSettings yesod)
#else
    , "build" .= ciSettings
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