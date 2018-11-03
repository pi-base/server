module Services.Github
  ( module Github
  , app
  , openPullRequest
  , getOAuthTokenFromCode
  , user
  ) where

import Core

import Control.Lens    hiding ((.=))
import Data.Aeson
import Data.Aeson.Lens

import Http
import Data.Store            (getDefaultBranch, pushBranch)
import Services.Github.Types as Github

app :: Core.Settings -> OAuth2
app s =
  let gh = s ^. githubSettings
  in OAuth2
      { clientId     = gh ^. Github.clientId
      , clientSecret = gh ^. Github.clientSecret
      , callbackUri  = gh ^. Github.callbackUri
      }

openPullRequest :: (Http m, Git m, MonadLogger m)
                => Github.Settings
                -> Branch
                -> m (Either Text Text)
openPullRequest settings branch = do
  pushBranch branch -- should already happen on branch update, but just to be sure

  pullRequestForBranch settings branch >>= \case
    Just url -> return $ Right url -- Already have an open pull request for branch
    Nothing  -> createPullRequestForBranch settings branch >>= \case
      Just url -> return $ Right url
      Nothing  -> return $ Left "Could not open pull request"

pullRequestForBranch :: (Http m, Git m)
                      => Github.Settings
                      -> Branch
                      -> m (Maybe Text)
pullRequestForBranch Github.Settings{..} Branch{..} = do
  let opts = defaults & header "Authorization" .~ ["token " <> encodeUtf8 _token]
                      & header "Accept" .~ ["application/vnd.github.v3+json"]
                      & param "head" .~ [_owner <> ":" <> branchName]
  r <- Http.get opts $ "https://api.github.com/repos/" <> _owner <> "/" <> _repo <> "/pulls"
  return $ r ^? nth 0 . key "html_url" . _String

createPullRequestForBranch :: (Http m, Git m)
                           => Github.Settings
                           -> Branch
                           -> m (Maybe Text)
createPullRequestForBranch Github.Settings{..} Branch{..} = do
  base <- getDefaultBranch
  r <- Http.post
         (defaults & header "Authorization" .~ ["token " <> encodeUtf8 _token]
                   & header "Accept" .~ ["application/vnd.github.v3+json"])
         ("https://api.github.com/repos/" <> _owner <> "/" <> _repo <> "/pulls")
         (toJSON $ object [ "title" .= branchName
                          , "head"  .= branchName
                          , "base"  .= base
                          ])
  return $ r ^? key "html_url" . _String

getOAuthTokenFromCode :: Http m => OAuth2 -> Text -> m (Maybe Text)
getOAuthTokenFromCode application code = do
  r <- Http.post
         (defaults & header "Accept" .~ ["application/json"])
         "https://github.com/login/oauth/access_token"
         (toJSON $ object [ "client_id"     .= Core.clientId application
                          , "client_secret" .= Core.clientSecret application
                          , "code"          .= code
                          ])
  return $ r ^? key "access_token" . _String

user :: Http m => AccessToken -> m (Maybe Github.User)
user accessToken = do
  let opts = defaults & header "Authorization" .~ ["token " <> encodeUtf8 accessToken]
                      & header "Accept" .~ ["application/vnd.github.v3+json"]
  response <- Http.get opts "https://api.github.com/user"
  return $ Github.User
    <$> (response ^? key "id" . _Integer)
    <*> (response ^? key "name" . _String)
    <*> (response ^? key "email" . _String)
