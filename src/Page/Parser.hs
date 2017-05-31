module Page.Parser
  ( Page(..)
  , Page.Parser.parse
  , write
  ) where

-- TODO: add round-trip quickchecks
-- TODO: replace manyTill w/ text-based parsers where possible

import Core

import qualified Data.Text             as T
import qualified Data.Yaml             as Y

import Control.Monad.Writer (execWriter, tell)
import Data.Aeson
import Data.Attoparsec.Text
import Data.Either.Combinators (mapLeft)

data Page a = Page
  { pagePath :: ByteString
  , pageFrontmatter :: a
  , pageMain :: Text
  , pageSections :: [(Text, Text)]
  }

parse :: FromJSON f => Record -> Either Error (Page f)
parse (path, content) = mapLeft (ParseError path) $ do
  (header, body) <- pullFrontmatter content
  front <- Y.decodeEither $ encodeUtf8 header
  (main, sections) <- parseSections body
  return $ Page path front main sections

write :: ToJSON a => Page a -> Record
write Page{..} = (,) pagePath . execWriter $ do
  tell "---\n"
  tell . decodeUtf8 $ Y.encode pageFrontmatter
  tell "---\n"
  tell pageMain
  tell "\n"
  forM pageSections $ \(header, contents) -> do
    tell $ "[[" <> header <> "]]"
    tell contents
    tell "\n"


pullFrontmatter :: Text -> Either String (Text, Text)
pullFrontmatter text = flip parseOnly text $ do
  fm   <- "---\n" *> manyTill anyChar "---\n"
  rest <- takeText
  return (T.pack fm, rest)

parseSections :: Text -> Either String (Text, [(Text, Text)])
parseSections body = flip parseOnly body $ do
  main_    <- manyTill anyChar endSection
  sections <- many' section
  return $ (t main_, sections)
  where
    endSection :: Parser ()
    endSection = void "[[" <|> endOfInput

    section :: Parser (Text, Text)
    section = do
      title <- manyTill anyChar "]]"
      body_ <- manyTill anyChar endSection
      return (t title, t body_)

    t :: String -> Text
    t = T.strip . T.pack

