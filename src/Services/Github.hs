module Services.Github
  ( openPullRequest
  ) where

import Protolude
import Core
import Data.Store (storeBaseBranch, pushBranch)
import Settings   (GithubSettings(..))

import           Control.Lens    hiding ((.=))
import           Data.Aeson
import           Data.Aeson.Lens
import qualified Data.Text       as T
import           Network.Wreq    as W

openPullRequest :: (MonadStore m, MonadLogger m)
                => GithubSettings
                -> Branch
                -> m (Either Text Text)
openPullRequest settings branch = do
  pushBranch branch -- should already happen on branch update, but just to be sure

  pullRequestForBranch settings branch >>= \case
    Just url -> return $ Right url -- Already have an open pull request for branch
    Nothing  -> createPullRequestForBranch settings branch >>= \case
      Just url -> return $ Right url
      Nothing  -> return $ Left "Could not open pull request"

pullRequestForBranch :: (MonadStore m, MonadLogger m)
                      => GithubSettings
                      -> Branch
                      -> m (Maybe Text)
pullRequestForBranch GithubSettings{..} Branch{..} = do
  let opts = defaults & header "Authorization" .~ ["token " <> encodeUtf8 gsToken]
                      & header "Accept" .~ ["application/vnd.github.v3+json"]
                      & param "head" .~ [gsOwner <> ":" <> branchName]
      path = T.unpack $ "https://api.github.com/repos/" <> gsOwner <> "/" <> gsRepo <> "/pulls"
  r <- liftIO $ getWith opts path
  return $ r ^? responseBody . nth 0 . key "html_url" . _String

createPullRequestForBranch :: (MonadStore m, MonadLogger m)
                           => GithubSettings
                           -> Branch
                           -> m (Maybe Text)
createPullRequestForBranch GithubSettings{..} Branch{..} = do
  base <- storeBaseBranch <$> getStore
  let opts = defaults & header "Authorization" .~ ["token " <> encodeUtf8 gsToken]
                      & header "Accept" .~ ["application/vnd.github.v3+json"]
      body = toJSON $ object [ "title" .= branchName
                             , "head"  .= branchName
                             , "base"  .= base
                             ]
      path = T.unpack $ "https://api.github.com/repos/" <> gsOwner <> "/" <> gsRepo <> "/pulls"
  r <- liftIO $ postWith opts path body
  return $ r ^? responseBody . key "html_url" . _String
