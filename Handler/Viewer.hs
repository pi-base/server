module Handler.Viewer where

import Import
import GitHub
import qualified GitHub.Data.Id as GH
import qualified GitHub.Endpoints.Issues.Comments as GH
import qualified GitHub.Endpoints.Repos.Statuses  as GH

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
  postStatus (pullRequestHead pr) GH.StatusError "Errors found"

prStatusOk :: PullRequest -> Handler ()
prStatusOk pr = do
  auth <- githubAuth
  let _id  = GH.Id $ pullRequestNumber pr
  issueComment _id "No errors found"
  postStatus (pullRequestHead pr) GH.StatusSuccess "No errors found"

githubAuth :: Handler GitHub.Auth
githubAuth = do
  settings <- appSettings <$> getYesod
  return $ appGitHubToken settings

issueComment :: GH.Id GH.Issue -> Text -> Handler ()
issueComment _id msg = do
  AppSettings{..} <- appSettings <$> getYesod
  liftIO $ GH.createComment appGitHubToken appGitHubOwner appGitHubRepo _id msg
  return ()

postStatus :: PullRequestCommit -> GH.StatusState -> Text -> Handler ()
postStatus commit state message = do
  AppSettings{..} <- appSettings <$> getYesod
  let sha = GH.mkCommitName $ pullRequestCommitSha commit
  s <- liftIO $ GH.createStatus appGitHubToken appGitHubOwner appGitHubRepo sha status
  return ()
  where
    status = GH.NewStatus state Nothing (Just message) (Just "pi-base/validator")

explainErrors :: [Core.Error] -> Text
explainErrors errors =
  "Mistakes were made\n```"
  <> unlines (map Core.explainError $ nub errors)
  <> "\n```"
