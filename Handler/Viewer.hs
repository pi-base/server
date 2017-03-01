module Handler.Viewer where

import Import

-- import Data.Aeson.Encode.Pretty (encodePretty)
import Data (Committish(..), storeMaster, parseViewer)
import Services.Github (checkPullRequest, webhookHandler)
import Viewer

getMaster :: Handler Viewer
getMaster = getYesod >>= storeMaster . appStore >>=
  either (sendStatusJSON status200) return

getViewerR :: Handler Value
getViewerR = getMaster >>= returnJson

getViewerSpacesR :: Handler Value
getViewerSpacesR = viewerSpaces <$> getMaster >>= returnJson

getViewerBranchR :: Text -> Handler Value
getViewerBranchR branch = do
  store <- appStore <$> getYesod
  ev    <- parseViewer store (Ref branch)
  returnJson ev

postHooksR :: Handler Value
postHooksR = do
  _pullRequest <- checkPullRequest <$> webhookHandler
  -- liftIO . BS.putStrLn $ encodePretty event
  returnJson ("ok"::Text)
