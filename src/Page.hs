module Page
  ( Page.Parser
  , Page.parse
  , P.write
  , withFrontmatter
  ) where

import Data.Aeson
import Data.Aeson.Types

import Data.Attoparsec.Text

import qualified Data.ByteString.Char8 as BS8
import qualified Data.Text as T
import qualified Data.Yaml as Y

import Core
import qualified Page.Parser as P

type Parser f a = P.Page f -> Either Error a

parse :: FromJSON f => (Page.Parser f a) -> TreeFilePath -> Text -> Either Error a
parse parser path contents = do
  page <- P.parse (path, contents)
  parser page

withFrontmatter :: (Y.Object -> Y.Parser a)
                -> (TreeFilePath, Text)
                -> Either Error a
withFrontmatter parser (path, content) = mapLeft (ParseError path) $ do
  frontmatter <- flip parseOnly content $ "---\n" *> manyTill anyChar "---\n"
  metadata    <- Y.decodeEither $ encodeUtf8 $ T.pack frontmatter
  Y.parseEither parser metadata

split :: FromJSON a => (TreeFilePath, Text) -> Either Error (a, Text)
split (path, content) = mapLeft (ParseError path) $ do
  (front, rest) <- flip parseOnly content $ do
    "---\n"
    front <- manyTill anyChar "---\n"
    rest  <- takeText
    return (front, rest)
  meta <- Y.decodeEither $ encodeUtf8 $ T.pack front
  return (meta, rest)

