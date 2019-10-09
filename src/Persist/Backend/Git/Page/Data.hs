{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Persist.Backend.Git.Page.Data
  ( Data'(..)
  , Data
  , headers
  , main
  , sections
  , parse
  , write
  ) where

import Core

import           Control.Monad.Writer (execWriter, tell)
import           Data.Aeson           (FromJSON(..), Object)
import           Data.Attoparsec.Text hiding (parse, space)
import           Data.String          (String)
import           Data.Structure       (HKD, LensFor(..), getLenses)
import qualified Data.HashMap.Strict  as HM
import qualified Data.Text            as T
import qualified Data.Yaml            as Y

data Data' f = Data
  { _headers  :: HKD f Object
  , _main     :: HKD f Text
  , _sections :: HKD f (HM.HashMap Text Text)
  } deriving (Generic)

type Data = Data' Identity

deriving instance Show Data
deriving instance Eq   Data

Data
  (LensFor headers)
  (LensFor main)
  (LensFor sections)
  = getLenses

-- TODO: refactor these implementations

parse :: Text -> Either ParseError Data
parse content = either (Left . ParseError) Right $ do
  (header, body) <- pullFrontmatter content
  front <- decodeYaml $ encodeUtf8 header
  (main', sections') <- parseSections body
  return $ Data front main' (HM.fromList sections')

write :: Data -> Text
write Data{..} = execWriter $ do
  tell "---\n"
  tell . decodeUtf8 $ Y.encode _headers
  tell "---\n"
  tell _main
  tell "\n"
  forM (HM.toList _sections) $ \(header, contents) -> do
    tell $ "[[" <> header <> "]]"
    tell "\n"
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
  sections' <- many' section
  return $ (t main_, sections')
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

decodeYaml :: FromJSON v => ByteString -> Either String v
decodeYaml = either (Left . show) Right . Y.decodeEither'