module Logger
  ( module Logger.Types
  , build
  , flush
  , log
  , logWithLoc
  , parseLogLevel
  , withLogging
  ) where

-- TODO: consider replacing logging internals with co-log
-- https://kowainik.github.io/posts/2018-09-25-co-log

import Protolude hiding (log, min)

import Control.Lens         ((^.))
import Control.Monad.Logger
import Data.String          (String)
import System.Log.FastLogger

import Logger.Types
import Settings

build :: Settings -> IO Logger
build settings = do
  loggerSet <- newStdoutLoggerSet defaultBufSize
  let loggerLevel = settings ^. logLevel
  return Logger{..}

-- TODO: when is _src set?
logWithLoc :: (MonadIO m, ToLogStr msg)
    => Logger
    -> Loc -> LogSource -> LogLevel -> msg
    -> m ()
logWithLoc logger loc src lvl msg = log logger lvl $ mconcat
  [ toLogStr msg
  , " -- "
  , location
  ]
  where
    location = mconcat $ if toLogStr src == ""
      then [ toLogStr (loc_filename loc)
           , ":"
           , toLogStr (show (fst $ loc_start loc) :: Text)
           ]
      else [ "["
           , toLogStr src
           , "]"
           ]

log :: (MonadIO m, ToLogStr msg) => Logger -> LogLevel -> msg -> m ()
log Logger{..} lvl msg = when (lvl >= loggerLevel) $
  liftIO $ pushLogStrLn loggerSet $ mconcat
    [ "["
    , toLogStr (levelTag lvl)
    , "] "
    , toLogStr msg
    ]

withLogging :: MonadIO m => Logger -> LoggingT m a -> m a
withLogging = flip runLoggingT . logWithLoc

flush :: MonadIO m => Logger -> m ()
flush = liftIO . flushLogStr . loggerSet

levelTag :: LogLevel -> Text
levelTag LevelError = "Error"
levelTag LevelWarn  = "Warn"
levelTag LevelInfo  = "Info"
levelTag LevelDebug = "Debug"
levelTag (LevelOther lvl) = lvl

parseLogLevel :: String -> Either String LogLevel
parseLogLevel "error" = Right LevelError
parseLogLevel "warn"  = Right LevelWarn
parseLogLevel "info"  = Right LevelInfo
parseLogLevel "debug" = Right LevelDebug
parseLogLevel other   = Left $ "Unknown log level: " <> other
