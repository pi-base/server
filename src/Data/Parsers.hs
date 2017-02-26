module Data.Parsers
  ( Record
  , parseSpace
  , parseProperty
  , parseTrait
  , parseTheorem
  ) where

import Core

import qualified Data.ByteString.Char8 as BS
import qualified Data.HashMap.Strict   as HM
import qualified Data.Text             as T
import qualified Data.Yaml             as Y

import Data.Aeson
import Data.Attoparsec.Text
import Data.Either.Combinators (mapLeft)
import Git (TreeFilePath)


-- TODO: replace manyTill w/ text-based parsers where possible
pullFrontmatter :: Text -> Either String (Text, Text)
pullFrontmatter text = flip parseOnly text $ do
  fm   <- "---\n" *> manyTill anyChar "---\n"
  rest <- takeText
  return (T.pack fm, rest)

parseSections :: Text -> Either String (HM.HashMap Text Value)
parseSections body = flip parseOnly body $ do
  main_ <- manyTill anyChar endSection
  sections <- many' section
  return . HM.fromList $ (k "", v main_) : sections
  where
    endSection :: Parser ()
    endSection = void "[[" <|> endOfInput

    section :: Parser (Text, Value)
    section = do
      title <- manyTill anyChar "]]"
      body_ <- manyTill anyChar endSection
      return (k title, v body_)

    k :: String -> Text
    k title = "[[" <> T.pack title <> "]]"

    v :: String -> Value
    v = String . T.strip . T.pack


withPage :: (Y.Object -> Y.Parser a) -> Record -> Either Error a
withPage parser (path, content) = mapLeft (ParseError path) $ do
  (front, body) <- pullFrontmatter content
  fm       <- Y.decodeEither $ encodeUtf8 front
  sections <- parseSections body
  let page = HM.union fm sections
  Y.parseEither parser page

main :: FromJSON a => Object -> Y.Parser a
main p = p .: "[[]]"


parseSpace :: Record -> Either Error Space
parseSpace = withPage $ \p ->
  Space
    <$> (SpaceId <$> p .: "uid")
    <*> p .: "slug"
    <*> p .: "name"
    <*> main p
    <*> p .:? "[[Proof of Topology]]"

parseProperty :: Record -> Either Error Property
parseProperty = withPage $ \p ->
  Property
    <$> (PropertyId <$> p .: "uid")
    <*> p .: "slug"
    <*> p .: "name"
    <*> p .:? "aliases"
    <*> main p

parseTheorem :: Record -> Either Error (Theorem Text)
parseTheorem = withPage $ \p ->
  Theorem
    <$> (TheoremId <$> p .: "uid")
    <*> p .: "if"
    <*> p .: "then"
    <*> (idify <$> p .:? "converse")
    <*> main p
  where
    idify Nothing = Nothing
    idify (Just ids) = Just $ map TheoremId ids

parseTrait :: Record -> Either Error (Trait Text Text, Maybe [Assumption])
parseTrait r = do
  (trait, raw) <- flip withPage r $ \p -> do
    trait <- Trait
      <$> (TraitId <$> p .: "uid")
      <*> p .: "space"
      <*> p .: "property"
      <*> p .: "value"
      <*> main p
    proof <- p .:? "[[Proof]]"
    return (trait, proof)
  case raw of
    Nothing -> Right (trait, Nothing)
    Just rp -> case parseProof rp of
      Left err   -> Left err
      Right pids -> Right (trait, Just pids)

assumptionFromId :: Text -> Assumption
assumptionFromId _id = case T.uncons _id of
  Just ('T', _) -> AssumedTrait $ TraitId _id
  _             -> AssumedTheorem $ TheoremId _id

parseProof :: Text -> Either Error [Assumption]
parseProof = mapLeft (ParseError "proof") . parseOnly (sepBy1 parseAssumption "\n")
  where
    parseAssumption = do
      _   <- "* ["
      _id <- takeTill $ \c -> c == ']'
      _   <- takeTill $ \c -> c == '\n'
      return $ assumptionFromId _id
