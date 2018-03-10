module Handler.Home where

import Import

import Data.Store      (getStoreBaseVersion)
import Handler.Helpers (generateToken)
import Services.Github (checkPullRequest, webhookHandler)
import Core            (View(..), unVersion)

getHomeR :: Handler Value
getHomeR = do
  version <- getStoreBaseVersion
  build   <- getSetting appBuild
  return $ object 
    [ "version" .= version
    , "build"   .= build
    ]

postHooksR :: Handler Value
postHooksR = do
  pullRequest <- webhookHandler
  result      <- checkPullRequest pullRequest
  let version = (maybe "??" unVersion . _viewVersion) <$> result
  either throw returnJson version

activeToken :: UserId -> Handler Token
activeToken userId = do
  token <- runDB $ selectFirst [TokenUserId ==. userId] [] -- TODO: not expired
  case token of
    Just (Entity _ t) -> return t
    _ -> generateToken userId

getFrontendR :: Handler ()
getFrontendR = do
  AppSettings{..} <- appSettings <$> getYesod
  defaultMaybeAuthId >>= \case
    Nothing -> redirect appFrontendUrl
    Just _id -> (runDB $ get _id) >>= \case
      Nothing -> redirect appFrontendUrl
      Just _ -> do
        Token{..} <- activeToken _id
        redirect $ appFrontendUrl <> "/login/" <> tokenUuid
