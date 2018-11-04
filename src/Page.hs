module Page
  ( build
  , Page.parse
  , updateMetadata
  , write
  ) where

import Protolude

import           Control.Monad.Writer    (execWriter, tell)
import           Control.Lens            hiding ((.=))
import qualified Data.Aeson.Types        as Aeson
import           Data.Attoparsec.Text
import           Data.Either.Combinators (mapLeft)
import qualified Data.HashMap.Strict     as HM
import qualified Data.Text               as T
import qualified Data.Yaml               as Y

import Core

parse :: Page a -> (TreeFilePath, Text) -> Either ParseError a
parse (Page p) (path, content) =
  parseData (path, content) >>= \page -> case page ^? p of
    Just a  -> Right a
    Nothing -> Left $ ParseError path "does not define a valid object"

write :: Page a -> a -> (TreeFilePath, Text)
write (Page p) page = writeData $ page ^. re p

build :: (a -> PageData) -> (PageData -> Aeson.Parser a) -> Page a
build t f' = Page $ prism t f
  where
    f p = mapLeft (const p) $ Aeson.parseEither f' p

parseData :: Record -> Either ParseError PageData
parseData (path, content) = mapLeft (ParseError path . T.pack) $ do
  (header, body) <- pullFrontmatter content
  front <- decodeYaml $ encodeUtf8 header
  (main, sections) <- parseSections body
  return $ PageData path front main (HM.fromList sections)

writeData :: PageData -> Record
writeData PageData{..} = (,) pagePath . execWriter $ do
  tell "---\n"
  tell . decodeUtf8 $ Y.encode pageFrontmatter
  tell "---\n"
  tell pageMain
  tell "\n"
  forM (HM.toList pageSections) $ \(header, contents) -> do
    tell $ "[[" <> header <> "]]"
    tell contents
    tell "\n"

-- TODO: replace manyTill w/ text-based parsers where possible

pullFrontmatter :: Text -> Either [Char] (Text, Text)
pullFrontmatter text = flip parseOnly text $ do
  fm   <- "---\n" *> manyTill anyChar "---\n"
  rest <- takeText
  return (T.pack fm, rest)

parseSections :: Text -> Either [Char] (Text, [(Text, Text)])
parseSections body = flip parseOnly body $ do
  main_    <- manyTill anyChar endSection
  sections <- many' section
  return $ (t main_, sections)
  where
    endSection :: Data.Attoparsec.Text.Parser ()
    endSection = void "[[" <|> endOfInput

    section :: Data.Attoparsec.Text.Parser (Text, Text)
    section = do
      title <- manyTill anyChar "]]"
      body_ <- manyTill anyChar endSection
      return (t title, t body_)

    t :: [Char] -> Text
    t = T.strip . T.pack

updateMetadata :: ((TreeFilePath, Aeson.Value) -> Either [Char] (TreeFilePath, Aeson.Value))
               -> (TreeFilePath, Text)
               -> Either ParseError (TreeFilePath, Text)
updateMetadata f (path, content) = mapLeft (ParseError path . T.pack) $ do
  (header, body)    <- pullFrontmatter content
  front             <- decodeYaml $ encodeUtf8 header
  (path', content') <- f (path, front)
  return (path', "---\n" <> decodeUtf8 (Y.encode content') <> "---\n" <> body)

decodeYaml :: FromJSON v => ByteString -> Either String v
decodeYaml = either (Left . show) Right . Y.decodeEither'
