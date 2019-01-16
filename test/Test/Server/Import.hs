module Test.Server.Import
  ( module X
  , Response(..)
  , header
  , get
  , makeApp
  , post
  ) where

import Test.Import as X hiding (put)

import Network.Wai as X (Application)
import Util        as X (encodeText)

import           Data.Aeson                (Value(Null), decode, encode)
import qualified Data.ByteString.Char8     as BS8
import           Data.CaseInsensitive      (CI(..), original)
import qualified Data.Map                  as Map
import qualified Data.Text                 as T
import           Network.HTTP.Types.Header (Header)
import           Network.HTTP.Types.Method (methodGet, methodPost)
import           Network.HTTP.Types.Status (statusCode)
import           Network.Wai.Test          (SResponse(..))
import qualified Test.Hspec.Wai            as Wai
import qualified Test.Hspec.Wai.Internal   as Wai

import qualified Server
import qualified Server.Middleware      as Middleware
import qualified Server.Middleware.Auth as Auth

data Response = Response
  { status  :: Int
  , headers :: Map Text String
  , body    :: Value
  } deriving (Show, Eq, Generic)

makeApp :: Runner -> TestM Application
makeApp run = do
  env <- ask

  middleware <- liftIO $ Middleware.def $ env ^. foundation

  let app = Server.build middleware (liftIO . run)

  return $ Auth.dummyMiddleware (env ^. userRef) $ app

get :: Application -> Text -> TestM Response
get app path = liftIO $ Wai.withApplication app $ do
  res <- Wai.request methodGet (encodeUtf8 path) requestHeaders ""
  return $ formatResponse res

post :: Application -> Text -> Value -> TestM Response
post app path body = liftIO $ Wai.withApplication app $ do
  res <- Wai.request methodPost (encodeUtf8 path) requestHeaders (encode body)
  return $ formatResponse res

requestHeaders :: [Header]
requestHeaders = [("Content-Type", "application/json")]

header :: Response -> Text -> String
header Response{..} name = fromMaybe "" $ Map.lookup (T.toUpper name) headers

formatResponse :: SResponse -> Response
formatResponse SResponse{..} = Response
  { status  = statusCode simpleStatus
  , headers = Map.fromList $ map (\(k, v) -> (normalizeHeader k, BS8.unpack v)) $ simpleHeaders
  , body    = fromMaybe Null $ decode simpleBody
  }

normalizeHeader :: CI ByteString -> Text
normalizeHeader = T.toUpper . decodeUtf8 . original
