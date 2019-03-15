{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Server.Api.Auth
  ( API
  , server
  ) where

import Server.Import hiding (header, state)

import           Control.Lens hiding ((.=))
import           Data.String  (String)
import qualified Data.UUID    as UUID
import qualified Data.UUID.V4 as UUID

import qualified Auth
import qualified Data.Branch               as Branch
import qualified Server.Middleware.Session as Session
import           Server.Util
import qualified Services.Github           as Github

type HTML = String
instance Accept HTML where
  contentType _ = "text/html; charset=utf-8"
instance MimeRender HTML () where
  mimeRender _ _ = ""

type API =
  Vault :>
    (    "github"
         :> QueryParam "returnUrl" Text
         :> Get '[HTML] ()
    :<|> "github" :> "callback"
         :> QueryParam "code" Text
         :> QueryParam "state" Text
         :> Get '[HTML] ()
    )

server :: App m => ServerT API m
server vault = githubStart vault
          :<|> githubCallback vault

-- TODO: track state as CSRF protection

githubStart :: App m => Vault -> Maybe Text -> m ()
githubStart vault returnUrl = do
  OAuth2{..} <- getApp

  Session.set vault returnUrlKey returnUrl

  state <- UUID.toText <$> liftIO UUID.nextRandom
  Session.set vault stateKey state

  redirectTo $ mconcat
    [ "https://github.com/login/oauth/authorize"
    , "?client_id=" <> clientId
    , "&response_type=code"
    , "&redirect_uri=" <> callbackUri
    , "&scope=user:email,public_repo"
    , "&state=" <> state
    ]

githubCallback :: App m => Vault -> Maybe Text -> Maybe Text -> m ()
githubCallback vault (Just code) state = do
  app <- getApp

  returnUrl   <- Session.delete vault returnUrlKey
  storedState <- Session.delete vault stateKey

  if state /= storedState
    then fail returnUrl "CSRF state did not match"
    else do
      Github.getOAuthTokenFromCode app code >>= \case
        Just accessToken ->
          ensureUserWithAccessToken accessToken >>= \case
            Left err -> fail returnUrl err
            Right (user, token) -> do
              $(logInfo) $ userName user <> " logged in"
              redirectToFrontend returnUrl $ "/login/" <> tokenUuid token
        _ -> fail returnUrl "Could not retreive access_token from code"

githubCallback vault _ _ = do
  returnUrl <- Session.delete vault returnUrlKey
  fail returnUrl "Did not receive a login code from Github"

getApp :: App m => m OAuth2
getApp = Github.app <$> view envSettings <$> getEnv

ensureUserWithAccessToken :: App m => Github.AccessToken -> m (Either Text (User, Token))
ensureUserWithAccessToken accessToken =
  Github.user accessToken >>= \case
    Nothing -> return $ Left "Could not find Github user"
    Just Github.User{..} -> do
      -- KLUDGE: we're rushing this one out and should rethink it
      --   What should we do if the user logs in later with more information?
      let user = User
                   { userName = fromMaybe ghUserLogin ghUserName
                   , userEmail = fromMaybe (ghUserLogin <> "@github.pi-base.org") ghUserEmail
                   , userIsReviewer = False
                   }
      id <- Auth.ensureIdent "github" (show ghUserId) accessToken user
      token <- Auth.generateToken id
      void $ Branch.ensureUserBranch $ Entity id user
      return $ Right (user, token)

-- TODO: make sure frontend displays these errors somehow
fail :: App m => Maybe Text -> Text -> m ()
fail viewerUrl message = do
  $(logInfo) message
  redirectToFrontend viewerUrl $ "?error=" <> message

returnUrlKey :: Text
returnUrlKey = "returnUrl"

stateKey :: Text
stateKey = "state"
