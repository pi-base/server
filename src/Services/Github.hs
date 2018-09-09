module Services.Github
  ( Github(..)
  , openPullRequest
  ) where

import Protolude hiding (get)
import Core
import Data.Store (storeBaseBranch, pushBranch)
import Settings   (GithubSettings(..))

import           Control.Lens    hiding ((.=))
import           Data.Aeson
import           Data.Aeson.Lens
import           Services.HTTP as HTTP

data Github = Github
  { http  :: HTTP
  , token :: Text
  , settings :: GithubSettings
  }

openPullRequest :: (MonadStore m, MonadLogger m)
                => Github
                -> Branch
                -> m (Either Text Text)
openPullRequest gh branch = do
  pushBranch branch -- should already happen on branch update, but just to be sure

  pullRequestForBranch gh branch >>= \case
    Just url -> return $ Right url -- Already have an open pull request for branch
    Nothing  -> createPullRequestForBranch gh branch >>= \case
      Just url -> return $ Right url
      Nothing  -> return $ Left "Could not open pull request"

pullRequestForBranch :: (MonadStore m, MonadLogger m)
                      => Github
                      -> Branch
                      -> m (Maybe Text)
pullRequestForBranch gh Branch{..} = do
  let GithubSettings{..} = settings gh
      opts = defaults & param "head" .~ [gsOwner <> ":" <> branchName]
      path = "repos/" <> gsOwner <> "/" <> gsRepo <> "/pulls"
  r <- request get gh path opts
  return $ r ^? responseBody . nth 0 . key "html_url" . _String

createPullRequestForBranch :: (MonadStore m, MonadLogger m)
                           => Github
                           -> Branch
                           -> m (Maybe Text)
createPullRequestForBranch gh Branch{..} = do
  let GithubSettings{..} = settings gh
  base <- storeBaseBranch <$> getStore
  let path = "repos/" <> gsOwner <> "/" <> gsRepo <> "/pulls"
      body = object [ "title" .= branchName
                    , "head"  .= branchName
                    , "base"  .= base
                    ]
  r <- request post gh path defaults body
  return $ r ^? responseBody . key "html_url" . _String

request :: (HTTP  -> Text -> HTTP.Options -> a)
        -> Github -> Text -> HTTP.Options -> a
request method gh path opts =
  method (http gh) ("https://api.github.com/" <> path) $
    opts & header "Authorization" .~ ["token " <> encodeUtf8 (token gh)]
         & header "Accept" .~ ["application/vnd.github.v3+json"]
