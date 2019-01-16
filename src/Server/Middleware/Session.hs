module Server.Middleware.Session
  ( delete
  , delete_
  , makeMiddleware
  , lookup
  , set
  ) where

import Server.Import

import           Data.Default            (def)
import qualified Data.Vault.Lazy         as Vault
import           Network.Wai             (Middleware)
import           Network.Wai.Session     (Session, withSession)
import           Network.Wai.Session.Map (mapStore_)
import           System.IO.Unsafe        (unsafePerformIO)
import           Web.Cookie              (setCookieHttpOnly, setCookiePath)

import Util (decodeText, encodeText)

sessionKey :: Vault.Key (Session IO Text Text)
sessionKey = unsafePerformIO Vault.newKey
{-# NOINLINE sessionKey #-}

lookup :: (MonadIO m, FromJSON a) => Vault -> Text -> m (Maybe a)
lookup vault key = case Vault.lookup sessionKey vault of
  Just (lookup', _) -> do
    found <- liftIO $ lookup' key
    return $ found >>= decodeText
  Nothing -> return Nothing

set :: (MonadIO m, ToJSON a) => Vault -> Text -> a -> m ()
set vault key value = case Vault.lookup sessionKey vault of
  Just (_, set') -> liftIO $ set' key $ encodeText value
  Nothing -> return ()

delete :: (MonadIO m, FromJSON a) => Vault -> Text -> m (Maybe a)
delete vault key = case Vault.lookup sessionKey vault of
  Just (lookup', set') -> liftIO $ do
    found <- lookup' key
    set' key ""
    return $ found >>= decodeText
  Nothing -> return Nothing

delete_ :: MonadIO m => Vault -> Text -> m ()
delete_ vault key = case Vault.lookup sessionKey vault of
  Just (_, set') -> liftIO $ set' key ""
  Nothing -> return ()

makeMiddleware :: IO Middleware
makeMiddleware = do
  store <- mapStore_
  let settings = def
        { setCookiePath     = Just "/"
        , setCookieHttpOnly = True
        }
  return $ withSession store "pi_base_session" settings sessionKey

