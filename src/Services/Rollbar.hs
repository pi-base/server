{-# LANGUAGE TemplateHaskell #-}
module Services.Rollbar
  ( HasRollbar(..)
  , Level(..)
  , Report(..)
  , Settings(..)
  , send
  , report
  ) where

import Import.NoFoundation

import           Data.Aeson   hiding (Error)
import qualified Network.Wai  as W
import           Network.Wreq (post)

import Services.Rollbar.Types

data Report = Report
  { message :: Text
  , context :: Maybe Text
  , level   :: Level
  , request :: Maybe YesodRequest
  , user    :: Maybe (Entity User)
  , custom  :: Maybe Object
  }

class MonadIO m => HasRollbar m where
  rollbar :: Level -> Text -> [(Text, Value)] -> m ()

report :: Report
report = Report
  { message = ""
  , context = Nothing
  , level   = Services.Rollbar.Types.Error
  , request = Nothing
  , user    = Nothing
  , custom  = Nothing
  }

send :: (MonadIO m, MonadLogger m) => Settings -> Report -> m ()
send Settings{..} Report{..} = when active $ case token of
  Nothing -> return ()
  Just t -> do
    $(logInfo) "Sending report to Rollbar"
    void $ liftIO $ post "https://api.rollbar.com/api/1/item/" $ object $
      [ "access_token" .= t
      , "data" .= object
        [ "body" .= object
          [ "message" .= object ["body" .= message]
          ]
        , "environment" .= environment
        , "level"        .= reportLevel level
        , "code_version" .= build
        , "language"     .= ("haskell" :: Text)
        , "framework"    .= ("yesod" :: Text)
        , "context"      .= context
        , "request"      .= fmap reportRequest request
        , "user"         .= fmap reportUser user
        , "notifier"     .= object
          [ "name"    .= ("pi-base" :: Text)
          , "version" .= ("0.0.1" :: Text)
          ],
          "custom" .= custom
        ]
      ]

reportLevel :: Level -> Text
reportLevel Critical = "critical"
reportLevel Error    = "error"
reportLevel Warning  = "warning"
reportLevel Info     = "info"
reportLevel Debug    = "debug"

reportRequest :: YesodRequest -> Value
reportRequest y = object
  [ "url"     .= decodeUtf8 (W.rawPathInfo w :: ByteString)
  , "method"  .= decodeUtf8 (W.requestMethod w :: ByteString)
  -- TODO: headers and GET / POST params where possible
  ]
  where 
    w = reqWaiRequest y

reportUser :: Entity User -> Value
reportUser (Entity _id User{..}) = object
  [ "id" .= _id
  , "username" .= userName
  , "email" .= userEmail
  ]