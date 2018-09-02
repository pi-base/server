{-# LANGUAGE RecordWildCards #-}
module Handler.User where

import Import
import Handler.Helpers

import Data.Aeson
import qualified Data.Branch as Branch

data Input = Input
  { ident :: Text
  , name :: Maybe Text
  , email :: Maybe Text
  }

instance FromJSON Input where
  parseJSON = withObject "Input" $ \o -> do
    ident <- o .: "ident"
    name  <- o .:? "name"
    email <- o .:? "email"
    return Input{..}

postUsersR :: Handler Value
postUsersR = do
  -- This is a test-only helper mode for creating users
  testing <- getSetting appTestMode
  unless testing notFound

  Input{..} <- requireJsonBody
  user <- ensureUser $ User
        { userIdent       = ident
        , userName        = maybe ident id name
        , userEmail       = maybe (ident <> "@example.com") id email
        , userGithubToken = "github:" <> ident
        }

  token <- generateToken $ entityKey user

  _ <- Branch.ensureUserBranch user

  returnJson $ object
    [ "user"  .= user
    , "token" .= token
    ]


