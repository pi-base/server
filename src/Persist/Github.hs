{-# LANGUAGE TemplateHaskell #-}
module Persist.Github
  ( Github
  , Config(..)
  , State
  , PullRequestError(..)
  , getOAuthTokenFromCode
  , initial
  , openPullRequest
  , pullRequests
  , runHttp
  , runState
  , toState
  , unreachable
  ) where

import Core

import           Control.Lens     ((^..), (^?), at)
import           Data.Aeson       ((.=), object, toJSON)
import           Data.Aeson.Lens  (_String, key, nth, values)
import qualified Data.Branch      as Branch
import qualified Data.Map         as Map
import           Data.PullRequest (PullRequest(..), PullRequestError(..))
import qualified Persist.Http     as Http
import           Persist.Http     (Http, defaults, header, param)
import           Polysemy.Reader  (Reader, ask, runReader)
import qualified Polysemy.State   as S (State)
import           Polysemy.State   (evalState, gets, modify)

data Config = Config
  { credentials :: OAuth2
  , token       :: Text
  , repo        :: Text
  , owner       :: Text
  } deriving (Show, Eq)

data Github m a where
  OpenPullRequest :: Branch -> Branch -> Github m (Either PullRequestError PullRequest)
  PullRequests    :: Github m [PullRequest]

makeSem ''Github

type State = Map Branch.Name PullRequest

initial :: State
initial = mempty

toState :: Sem (Github ': r) a -> Sem (S.State State ': r) a
toState = reinterpret \case
  OpenPullRequest from _ -> do
    let pr = PullRequest $ "http://example.com/pulls/" <> Branch._name from
    modify $ at (Branch._name from) .~ Just pr
    return $ Right pr

  PullRequests -> gets Map.elems

runState :: Sem (Github ': r) a -> Sem r a
runState = evalState mempty . toState

unreachable :: Sem (Github ': r) a -> Sem r a
unreachable = interpret \case
  OpenPullRequest _ _ -> return $ Left $ PullRequestError "unreachable"
  PullRequests        -> return []

runHttp :: Member Http r => Config -> Sem (Github ': r) a -> Sem r a
runHttp config = interpret \case
  OpenPullRequest from into -> runReader config $ httpOpenPullRequest from into
  PullRequests -> runReader config httpPullRequests

httpPullRequests :: Members '[Http, Reader Config] r => Sem r [PullRequest]
httpPullRequests = do
  Config{..} <- ask
  let opts = defaults & header "Authorization" .~ ["token " <> encodeUtf8 token]
                      & header "Accept" .~ ["application/vnd.github.v3+json"]
  r <- Http.get opts $ "https://api.github.com/repos/" <> owner <> "/" <> repo <> "/pulls"
  return $ map PullRequest $ r ^.. values . key "html_url" . _String

httpOpenPullRequest :: Members '[Http, Reader Config] r
                    => Branch
                    -> Branch
                    -> Sem r (Either PullRequestError PullRequest)
httpOpenPullRequest from into = do
  -- TODO: pushBranch branch -- should already happen on branch update, but just to be sure

  pullRequestForBranch from >>= \case
    Just url -> return $ Right url -- Already have an open pull request for branch
    Nothing  -> createPullRequestForBranch from into >>= \case
      Just url -> return $ Right url
      Nothing  -> return $ Left $ PullRequestError "Could not open pull request"

pullRequestForBranch :: Members '[Http, Reader Config] r
                     => Branch
                     -> Sem r (Maybe PullRequest)
pullRequestForBranch branch = do
  Config{..} <- ask
  let opts = defaults & header "Authorization" .~ ["token " <> encodeUtf8 token]
                      & header "Accept" .~ ["application/vnd.github.v3+json"]
                      & param "head" .~ [owner <> ":" <> Branch._name branch]
  r <- Http.get opts $ "https://api.github.com/repos/" <> owner <> "/" <> repo <> "/pulls"
  return $ PullRequest <$> r ^? nth 0 . key "html_url" . _String

createPullRequestForBranch :: Members '[Http, Reader Config] r
                           => Branch
                           -> Branch
                           -> Sem r (Maybe PullRequest)
createPullRequestForBranch from into = do
  Config{..} <- ask
  r <- Http.post
         (defaults & header "Authorization" .~ ["token " <> encodeUtf8 token]
                   & header "Accept" .~ ["application/vnd.github.v3+json"])
         ("https://api.github.com/repos/" <> owner <> "/" <> repo <> "/pulls")
         (toJSON $ object [ "title" .= Branch._name from
                          , "head"  .= Branch._name from
                          , "base"  .= Branch._name into
                          ])
  -- TODO: parse out and return error
  return $ PullRequest
    <$> r ^? key "html_url" . _String

getOAuthTokenFromCode :: Members '[Http] r
                      => OAuth2
                      -> Text
                      -> Sem r (Maybe Text)
getOAuthTokenFromCode application code = do
  r <- Http.post
         (defaults & header "Accept" .~ ["application/json"])
         "https://github.com/login/oauth/access_token"
         (toJSON $ object [ "client_id"     .= Core.clientId application
                          , "client_secret" .= Core.clientSecret application
                          , "code"          .= code
                          ])
  return $ r ^? key "access_token" . _String

-- user :: Members '[Http] r => AccessToken -> Sem r (Maybe Github.User)
-- user accessToken = do
--   let opts = defaults & header "Authorization" .~ ["token " <> encodeUtf8 accessToken]
--                       & header "Accept" .~ ["application/vnd.github.v3+json"]
--   response <- Http.get opts "https://api.github.com/user"

--   return $ Github.User
--     <$> response ^? key "id" . _Integer
--     <*> response ^? key "login" . _String
--     <*> pure (response ^? key "name" . _String)
--     <*> pure (response ^? key "email" . _String)
