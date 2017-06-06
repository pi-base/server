module Handler.Home where

import Import

import Data            (storeMaster, parseViewer)
import Handler.Helpers (createToken)
import Services.Github (checkPullRequest, webhookHandler)
import Core            (unVersion)
import Viewer

getMaster :: Handler Viewer
getMaster = storeMaster >>=
  either (sendStatusJSON status200) return

getHomeR :: Handler Value
getHomeR = do
  -- version <- getVersion $ Branch "master"
  let version = error "version" :: Text
  return $ object [ "version" .= version ]

postHooksR :: Handler Value
postHooksR = do
  pullRequest <- webhookHandler
  result      <- checkPullRequest pullRequest
  returnJson $ (unVersion . viewerVersion) <$> result

activeToken :: UserId -> Handler Token
activeToken userId = do
  token <- runDB $ selectFirst [TokenUserId ==. userId] [] -- TODO: not expired
  case token of
    Just (Entity _ t) -> return t
    _ -> createToken userId

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
