module Handler.Viewer where

import Import
import GitHub
import qualified GitHub.Data.Id as GH
import qualified GitHub.Endpoints.Issues.Comments as GH

import qualified Core
import Data (Committish(..), storeMaster, parseViewer, fetchPullRequest)
import Viewer

-- FIXME: remove
import Data.List (nub)
import qualified Data.ByteString.Lazy as BS
import Data.Aeson.Encode.Pretty (encodePretty)
import Handler.Home
getHooksR = getHomeR

getMaster = do
  store <- appStore <$> getYesod
  storeMaster store >>= \case
    Left err     -> sendStatusJSON status200 err
    Right viewer -> return viewer

getViewerR :: Handler Value
getViewerR = do
  master <- getMaster
  returnJson master

getViewerSpacesR :: Handler Value
getViewerSpacesR = do
  master <- getMaster
  returnJson $ viewerSpaces master

getViewerBranchR :: Text -> Handler Value
getViewerBranchR branch = do
  store  <- appStore <$> getYesod
  viewer <- parseViewer store $ Ref branch
  case viewer of
    Left  errs   -> returnJson errs
    Right viewer -> returnJson viewer

postHooksR :: Handler Value
postHooksR = do
  event <- requireJsonBody :: Handler PullRequestEvent
  -- TODO: validate secret
  checkPR event
  let pr = pullRequestEventPullRequest event
  -- liftIO . BS.putStrLn $ encodePretty event
  returnJson $ pullRequestPatchUrl pr

checkPR :: PullRequestEvent -> Handler ()
checkPR pre = do
  let
    pr   = pullRequestEventPullRequest pre
    _id  = GH.Id $ pullRequestNumber pr
    head = pullRequestHead pr
    ref  = pullRequestCommitRef head
    sha  = pullRequestCommitSha head

  fetchPullRequest ref
  store <- appStore <$> getYesod
  ev    <- parseViewer store $ Sha sha
  case ev of
    Left  errs   -> prStatusError pr errs
    Right viewer -> prStatusOk pr
  return ()

prStatusError :: PullRequest -> [Core.Error] -> Handler ()
prStatusError pr errors = do
  auth <- githubAuth
  let _id  = GH.Id $ pullRequestNumber pr
  issueComment _id $ explainErrors errors

prStatusOk :: PullRequest -> Handler ()
prStatusOk pr = do
  auth <- githubAuth
  let _id  = GH.Id $ pullRequestNumber pr
  issueComment _id "No errors found!"

githubAuth :: Handler GitHub.Auth
githubAuth = do
  settings <- appSettings <$> getYesod
  return $ appGitHubToken settings

issueComment :: GH.Id GH.Issue -> Text -> Handler ()
issueComment _id msg = do
  AppSettings{..} <- appSettings <$> getYesod
  liftIO $ GH.createComment appGitHubToken appGitHubOwner appGitHubRepo _id msg
  return ()

explainErrors :: [Core.Error] -> Text
explainErrors errors =
  "Mistakes were made\n```"
  <> unlines (map Core.explainError $ nub errors)
  <> "\n```"
