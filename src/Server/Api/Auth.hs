{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Server.Api.Auth
  ( API
  , server
  ) where

import Server.Import hiding (header)

import           Control.Lens hiding ((.=))
import           Data.String (String)

import qualified Auth
import           Server.Util
import qualified Services.Github as Github

type HTML = String
instance Accept HTML where
  contentType _ = "text/html; charset=utf-8"
instance MimeRender HTML () where
  mimeRender _ _ = ""

type API = "github" :> Get '[HTML] ()
      :<|> "github" :> "callback" :> QueryParam "code" Text :> Get '[HTML] ()

server :: App m => ServerT API m
server = githubStart
    :<|> githubCallback

-- TODO: track state as CSRF protection
-- TODO: move into Services.Github

githubStart :: App m => m ()
githubStart = do
  OAuth2{..} <- getApp
  redirectTo $ mconcat
    [ "https://github.com/login/oauth/authorize"
    , "?client_id=" <> clientId
    , "&response_type=code"
    , "&redirect_uri=" <> callbackUri
    , "&scope=user:email,public_repo"
    ]

githubCallback :: App m => Maybe Text -> m ()
githubCallback (Just code) = do
  app <- getApp
  Github.getOAuthTokenFromCode app code >>= \case
    Just accessToken ->
      ensureUserWithAccessToken accessToken >>= \case
        Left err -> fail err
        Right (user, token) -> do
          $(logInfo) $ userName user <> " logged in"
          redirectToFrontend $ "/login/" <> tokenUuid token
    _ -> fail "Could not retreive access_token from code"

githubCallback _ = do
  let message = "Did not receive a login code from Github"
  $(logInfo) message
  redirectToFrontend $ "?error=" <> message

getApp :: App m => m OAuth2
getApp = Github.app <$> view envSettings <$> getEnv

ensureUserWithAccessToken :: App m => Github.AccessToken -> m (Either Text (User, Token))
ensureUserWithAccessToken accessToken =
  Github.user accessToken >>= \case
    Nothing -> return $ Left "Could not find Github user"
    Just Github.User{..} -> do
      let user = User
                   { userName = ghUserName
                   , userEmail = ghUserEmail
                   , userIsReviewer = False
                   }
      id <- Auth.ensureIdent "github" (show ghUserId) accessToken user
      token <- Auth.generateToken id
      return $ Right (user, token)

-- TODO: make sure frontend displays these errors somehow
fail :: App m => Text -> m ()
fail message = do
  $(logInfo) message
  redirectToFrontend $ "?error=" <> message
