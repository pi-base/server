module Handler.Home where

import Import

import Data (Committish(..), storeMaster, parseViewer)
import Services.Github (checkPullRequest, webhookHandler)
import Viewer

getMaster :: Handler Viewer
getMaster = getYesod >>= storeMaster . appStore >>=
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

getFrontendR :: Handler ()
getFrontendR = do
  (Entity _id u) <- requireAuth
  redirect $ "https://localhost:3000/?token=" <> userGithubToken u

getMeR :: Handler Value
getMeR = do
  user <- requireAuth
  returnJson user
