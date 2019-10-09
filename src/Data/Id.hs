{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
module Data.Id
  ( Id(..)
  , Encodable(..)
  , decode
  , encode
  , format
  , fromInt
  , parseJSON
  , generate
  , parse
  , toJSON
  ) where

import Import

import           Control.Monad        (fail)
import qualified Data.Aeson           as Aeson (Value(..), withText)
import qualified Data.Aeson.Types     as Aeson (Parser)
import           Data.Attoparsec.Text hiding (parse)
import           Data.Char            (toLower, toUpper)
import           Data.Text            (cons, justifyRight, pack)
import qualified Data.UUID            as UUID
import qualified Data.UUID.V4         as UUID

data Id a
  = CanonicalId Int
  | TemporaryId UUID
  deriving (Generic, Eq, Ord, Show)

format :: Char -> Id a -> Text
format c (CanonicalId n) =
  cons (toUpper c) $ justifyRight 6 '0' $ show n
format c (TemporaryId u) =
  cons (toLower c) $ UUID.toText u

fromInt :: Int -> Id a
fromInt = CanonicalId

generate :: MonadIO m => m (Id a)
generate = TemporaryId <$> liftIO UUID.nextRandom

parse :: String -> Text -> Either Text (Id a)
parse chars = either (Left . pack) Right . parseOnly (parser chars)

parseJSON :: String -> Aeson.Value -> Aeson.Parser (Id a)
parseJSON chars = Aeson.withText "id" $ \text ->
  case parse chars text of
    Right id -> return id
    _ -> fail "Could not parse id"

toJSON :: Char -> Id a -> Aeson.Value
toJSON c = Aeson.String . format c

parser :: String -> Parser (Id a)
parser chars = canonical chars <|> temporary chars

canonical :: String -> Parser (Id a)
canonical chars = do
  _ <- satisfy (inClass $ map toUpper chars)
  _ <- many "0"
  n <- decimal
  return $ fromInt n

temporary :: String -> Parser (Id a)
temporary chars = do
  _ <- satisfy (inClass $ map toLower chars)
  t <- takeText
  case UUID.fromText t of
    Just u  -> return $ TemporaryId u
    Nothing -> fail "Could not parse UUID"

class Encodable a where
  prefix :: Char

encode :: forall a. Encodable a => Id a -> Text
encode = format (prefix @a)

decode :: forall a. Encodable a => Text -> Either Text (Id a)
decode = parse [prefix @a]