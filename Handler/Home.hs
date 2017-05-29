module Handler.Home where

import Import

import Data (Committish(..), storeMaster, parseViewer)
import Handler.Helpers (createToken)
import Services.Github (checkPullRequest, webhookHandler)
import Viewer

getMaster :: Handler Viewer
getMaster = storeMaster >>=
  either (sendStatusJSON status200) return

getHomeR :: Handler Value
getHomeR = do
  viewer <- getMaster
  return $ object
    [ ("version" :: Text) .= viewerVersion viewer
    ]

getViewerR :: Handler Value
getViewerR = getMaster >>= returnJson

getViewerBranchR :: Text -> Handler Value
getViewerBranchR branch = do
  store <- appStore <$> getYesod
  ev    <- parseViewer store (Ref branch)
  returnJson ev

postHooksR :: Handler Value
postHooksR = do
  pullRequest <- webhookHandler
  result      <- checkPullRequest pullRequest
  returnJson $ viewerVersion <$> result

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
        Token{..}       <- activeToken _id
        redirect $ appFrontendUrl <> "/login/" <> tokenUuid
