module Services.Github
  ( checkPullRequest
  , webhookHandler
  ) where

import Import
import qualified Core (Error, explainError)
import Data (parseViewer, fetchPullRequest)
import Types (Viewer, Committish(..))

import Control.Monad (unless)
import Crypto.Hash
import Data.Aeson (eitherDecode)
import qualified Data.ByteString.Char8 as BC8
import qualified Data.ByteString.Lazy  as LBS
import Data.List (nub) -- FIXME: remove this
import qualified Data.Text as T
import GitHub
import qualified GitHub.Data.Id as GH
import qualified GitHub.Endpoints.Issues.Comments as GH
import qualified GitHub.Endpoints.Repos.Statuses  as GH

webhookHandler :: FromJSON a => Handler a
webhookHandler = do
  body <- rawRequestBody $$ runCatchC foldC
  str  <- either halt return body
  validateSignature str
  either halt return . eitherDecode $ LBS.fromStrict str

checkPullRequest :: PullRequestEvent -> Handler (Either [Core.Error] Viewer)
checkPullRequest pre = do
  let
    pr   = pullRequestEventPullRequest pre
    _id  = GH.Id $ pullRequestNumber pr
    sha  = pullRequestCommitSha $ pullRequestHead pr
    _sha = GH.mkCommitName sha

  foundation <- getYesod
  fetchPullRequest (appRepoPath $ appSettings foundation)
  result <- parseViewer $ Sha sha
  either (prStatusError _id _sha) (prStatusOk _id _sha) result
  return result

validateSignature :: ByteString -> Handler ()
validateSignature body = do
  secret <- getSetting appGitHubWebhookSecret
  lookupHeader "X-Hub-Signature" >>= \case
    Nothing -> halt ("No signature found" :: Text)
    Just given ->
      unless (signaturesMatch body secret given) $
        halt ("invalid secret" :: Text)

halt :: Show a => a -> Handler b
halt msg = sendStatusJSON status400 ("Invalid webhook: " <> tshow msg)

signaturesMatch :: ByteString -> Text -> ByteString -> Bool
signaturesMatch body key given = comparison == computed
  where
    -- TODO: eq instance for HMAC is constant-time, and we should use it
    --   not clear how to convert str -> HMAC SHA1 (without hashing) though
    -- computed :: HMAC SHA1
    -- computed = hmac (BC8.pack $ T.unpack key) body

    computed :: Maybe ByteString
    computed = Just . digestToHexByteString . hmacGetDigest $ (hmac (BC8.pack $ T.unpack key) body :: HMAC SHA1)

    comparison :: Maybe ByteString
    comparison = BC8.stripPrefix "sha1=" given

prStatusError :: Id Issue -> Name Commit -> [Core.Error] -> Handler ()
prStatusError _id sha errors = do
  issueComment _id $ explainErrors errors
  postStatus sha GH.StatusError "Errors found"

prStatusOk :: Id Issue -> Name Commit -> Viewer -> Handler ()
prStatusOk _id sha _viewer = do
  issueComment _id "No errors found"
  postStatus sha GH.StatusSuccess "No errors found"

issueComment :: GH.Id GH.Issue -> Text -> Handler ()
issueComment _id msg = do
  AppSettings{..} <- appSettings <$> getYesod
  _ <- liftIO $ GH.createComment appGitHubToken appGitHubOwner appGitHubRepo _id msg
  return ()

postStatus :: Name Commit -> GH.StatusState -> Text -> Handler ()
postStatus sha state message = do
  AppSettings{..} <- appSettings <$> getYesod
  _ <- liftIO $ GH.createStatus appGitHubToken appGitHubOwner appGitHubRepo sha status
  return ()
  where
    status = GH.NewStatus state Nothing (Just message) (Just "pi-base/validator")

explainErrors :: [Core.Error] -> Text
explainErrors errors =
  "Mistakes were made\n```"
  <> unlines (map Core.explainError $ nub errors)
  <> "\n```"
