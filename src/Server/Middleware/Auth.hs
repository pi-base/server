module Server.Middleware.Auth
  ( middleware
  , dummyMiddleware
  , requestUser
  , vaultUser
  ) where

import Server.Import

import qualified Data.ByteString as BS
import qualified Data.Vault.Lazy as V
import           Network.Wai
import           System.IO.Unsafe (unsafePerformIO)

import qualified Auth

userKey :: V.Key (Entity User)
userKey = unsafePerformIO V.newKey
{-# NOINLINE userKey #-}

requestUser :: Request -> Maybe (Entity User)
requestUser = vaultUser . vault

vaultUser :: Vault -> Maybe (Entity User)
vaultUser = V.lookup userKey

middleware :: Env
           -> Application
           -> Request
           -> (Response -> IO ResponseReceived)
           -> IO ResponseReceived
middleware env = lookupUserBy $ \req ->
  runApp env $ maybe (return Nothing) Auth.userWithToken $ requestToken req

dummyMiddleware :: IORef (Maybe (Entity User)) -> Middleware
dummyMiddleware ref = lookupUserBy $ \_ -> readIORef ref

lookupUserBy :: (Request -> IO (Maybe (Entity User))) -> Middleware
lookupUserBy action app req next = do
  muser <- action req
  let req' = case muser of
               Just user -> req { vault = V.insert userKey user $ vault req }
               Nothing -> req
  app req' next

requestToken :: Request -> Maybe AuthToken
requestToken req =
  case find (\(name, _) -> name == "Authorization") $ requestHeaders req of
    Nothing -> Nothing
    Just (_, tok) -> decodeUtf8 <$> stripPrefix "Bearer " tok

stripPrefix :: ByteString -> ByteString -> Maybe ByteString
stripPrefix pre body = if pre `BS.isPrefixOf` body
  then Just $ BS.drop (BS.length pre) body
  else Nothing
