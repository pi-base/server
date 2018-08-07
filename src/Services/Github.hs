{-# LANGUAGE TemplateHaskell #-}
module Services.Github
  ( checkPullRequest
  , createPullRequest
  , webhookHandler
  ) where

import Import
import Core       hiding (Id, Commit)
import Data.Store (storeBaseBranch, fetchBranches, pushBranch)

import           Crypto.Hash
import           Data.Aeson                       (eitherDecode)
import qualified Data.ByteString.Char8            as BC8
import qualified Data.ByteString.Lazy             as LBS
import qualified Data.Text                        as T
import           GitHub                           as GH
import qualified GitHub.Data.Id                   as GH
import qualified GitHub.Endpoints.Issues.Comments as GH
import qualified GitHub.Endpoints.Repos.Statuses  as GH
import qualified GitHub.Endpoints.PullRequests    as GH

createPullRequest :: (MonadStore m, MonadLogger m) 
                  => GithubSettings -> Core.Branch 
                  -> m (Either Text Text) -- TODO: error type instead of Text
createPullRequest GithubSettings{..} branch = do
  pushBranch branch -- should already happen on branch update, but just to be sure

  base <- storeBaseBranch <$> getStore
  let name = Core.branchName branch
      request = CreatePullRequest
        { createPullRequestTitle = name
        , createPullRequestBody = ""
        , createPullRequestHead = name
        , createPullRequestBase = base
        }
  epr <- liftIO $ GH.createPullRequest gsToken gsOwner gsRepo request
  case epr of
    Left err -> do
      $(logError) $ T.pack $ show err
      return $ Left "Failed to open pull request" -- TODO: better error messaging (but don't leak token)
    Right pr -> return $ Right . GH.getUrl $ GH.pullRequestHtmlUrl pr

-- TODO: deprecate / cleanup below vvv

webhookHandler :: FromJSON a => Handler a
webhookHandler = do
  body <- runConduit $ rawRequestBody .| runCatchC foldC
  str  <- either halt return body
  validateSignature str
  either halt return . eitherDecode $ LBS.fromStrict str

checkPullRequest :: PullRequestEvent -> Handler (Either Core.ValidationError View)
checkPullRequest pre = do
  let
    pr   = pullRequestEventPullRequest pre
    _id  = GH.Id $ pullRequestNumber pr
    sha  = pullRequestCommitSha $ pullRequestHead pr
    _sha = GH.mkCommitName sha

  fetchBranches
  result <- error "parseViewer" $ CommitSha sha
  either (prStatusError _id _sha) (prStatusOk _id _sha) result
  return result

validateSignature :: ByteString -> Handler ()
validateSignature body = do
  secret <- getSetting $ gsWebhookSecret . appGithub
  lookupHeader "X-Hub-Signature" >>= \case
    Nothing -> halt ("No signature found" :: Text)
    Just given ->
      unless (signaturesMatch body secret given) $
        halt ("invalid secret" :: Text)

halt :: Show a => a -> Handler b
halt msg = sendStatusJSON status400 ("Invalid webhook: " <> show msg)

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

prStatusError :: Id Issue -> Name Commit -> Core.ValidationError -> Handler ()
prStatusError _id sha errors = do
  issueComment _id $ explainErrors errors
  postStatus sha GH.StatusError "Errors found"

prStatusOk :: Id Issue -> Name Commit -> View -> Handler ()
prStatusOk _id sha _viewer = do
  issueComment _id "No errors found"
  postStatus sha GH.StatusSuccess "No errors found"

issueComment :: GH.Id GH.Issue -> Text -> Handler ()
issueComment _id msg = do
  GithubSettings{..} <- getSetting appGithub
  _ <- liftIO $ GH.createComment gsToken gsOwner gsRepo _id msg
  return ()

postStatus :: Name Commit -> GH.StatusState -> Text -> Handler ()
postStatus sha state message = do
  GithubSettings{..} <- getSetting appGithub
  _ <- liftIO $ GH.createStatus gsToken gsOwner gsRepo sha status
  return ()
  where
    status = GH.NewStatus state Nothing (Just message) (Just "pi-base/validator")

explainErrors :: Core.ValidationError -> Text
explainErrors _ = error "explainErrors"
  -- "Mistakes were made\n```"
  -- <> unlines (map Core.explainError $ nub errors)
  -- <> "\n```"
