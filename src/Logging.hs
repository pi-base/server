module Logging
  ( makeLogWare
  ) where

import Import

import qualified Data.ByteString       as BS
import qualified Data.ByteString.Char8 as BS8

import Network.Wai
import Network.Wai.Middleware.RequestLogger
import System.Console.ANSI as Logging
import System.Log.FastLogger (toLogStr)

makeLogWare :: App -> IO Middleware
makeLogWare foundation = mkRequestLogger def
  { outputFormat = CustomOutputFormat $ outputFormatter foundation
  , destination = Logger . loggerSet $ appLogger foundation
  }

outputFormatter :: App -> OutputFormatter
outputFormatter _foundation _date req status _msize =
  mconcat $ map toLogStr $
       ansiMethod (requestMethod req) 
    ++ [" ", rawPathInfo req, " "]
    ++ ansiStatusCode (statusCode status) statusMessage' 
    ++ ["\n"]
  where
    statusMessage' = (BS8.pack . show $ statusCode status) <> " " <> statusMessage status

-- Color helpers copied from Network.Wai.Middleware.RequestLogger

ansiColor :: Color -> BS.ByteString -> [BS.ByteString]
ansiColor color bs =
    [ BS8.pack $ setSGRCode [SetColor Foreground Dull color]
    , bs
    , BS8.pack $ setSGRCode [Reset]
    ]

ansiMethod :: BS.ByteString -> [BS.ByteString]
ansiMethod m = case m of
    "GET"    -> ansiColor Cyan m
    "HEAD"   -> ansiColor Cyan m
    "PUT"    -> ansiColor Green m
    "POST"   -> ansiColor Yellow m
    "DELETE" -> ansiColor Red m
    _        -> ansiColor Magenta m

ansiStatusCode :: Int -> BS.ByteString -> [BS.ByteString]
ansiStatusCode c t = case take 1 $ show c of
    "2" -> ansiColor Green t
    "3" -> ansiColor Yellow t
    "4" -> ansiColor Red t
    "5" -> ansiColor Magenta t
    _   -> ansiColor Blue t