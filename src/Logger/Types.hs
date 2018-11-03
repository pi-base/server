module Logger.Types
  ( Logger(..)
  , LogLevel(..)
  ) where

import Control.Monad.Logger  (LogLevel(..))
import System.Log.FastLogger (LoggerSet)

data Logger = Logger
  { loggerSet   :: LoggerSet
  , loggerLevel :: LogLevel
  }
