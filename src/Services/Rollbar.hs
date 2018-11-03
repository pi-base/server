{-# LANGUAGE TemplateHaskell #-}
module Services.Rollbar
  ( HasRollbar(..)
  , Report(..)
  , Types.Settings(..)
  , report
  , send
  ) where

import Core

import           Control.Monad.Logger (LogLevel(..))
import           Data.Aeson           hiding (Error)
import           Network.Wai

import           Http
import qualified Logger
import           Services.Rollbar.Types as Types

data Report = Report
  { message :: Text
  , context :: Maybe Text
  , level   :: LogLevel
  , request :: Maybe Request
  , user    :: Maybe (Entity User)
  , custom  :: Maybe Object
  }

class MonadIO m => HasRollbar m where
  rollbar :: LogLevel -> Text -> [(Text, Value)] -> m ()

report :: Report
report = Report
  { message = ""
  , context = Nothing
  , level   = LevelError
  , request = Nothing
  , user    = Nothing
  , custom  = Nothing
  }

send :: (Http m, MonadUnliftIO m) => Types.Settings -> Logger.Logger -> Report -> m ()
send settings logger Report{..} = if isJust (settings ^. token)
  then do
    Logger.withLogging logger $
      $(logError) $ "Sending to Rollbar - " <> message

    u <- askUnliftIO
    void . liftIO . forkIO $
      unliftIO u $
      void $ Http.post defaults "https://api.rollbar.com/api/1/item/" body
  else
    Logger.withLogging logger $ do
      $(logError) message
      $(logDebug) $ pj body
  where
    body :: Value
    body = object
      [ "access_token" .= (settings ^. token)
      , "data" .= object
        [ "body" .= object
          [ "message" .= object ["body" .= message]
          ]
        , "environment"  .= (settings ^. environment)
        , "level"        .= reportLevel level
        , "code_version" .= (settings ^. build)
        , "language"     .= ("haskell" :: Text)
        , "framework"    .= ("haskell" :: Text)
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

reportLevel :: LogLevel -> Text
reportLevel  LevelError    = "error"
reportLevel  LevelWarn     = "warning"
reportLevel  LevelInfo     = "info"
reportLevel  LevelDebug    = "debug"
reportLevel (LevelOther x) = x

reportRequest :: Request -> Value
reportRequest req = object
  [ "url"     .= decodeUtf8 (rawPathInfo req)
  , "method"  .= decodeUtf8 (requestMethod req)
  -- TODO: fetch request body?
  ]

reportUser :: Entity User -> Value
reportUser (Entity _id User{..}) = object
  [ "id" .= _id
  , "username" .= userName
  , "email" .= userEmail
  ]
