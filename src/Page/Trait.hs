{-# LANGUAGE DeriveGeneric #-}
module Page.Trait
  ( Page.Trait.parse
  , parser
  , path
  , write
  ) where

import           Data.Aeson
import           Data.Attoparsec.Text hiding (space)
import qualified Data.HashMap.Strict  as HM
import qualified Data.Set             as S

import Core
import Page.Parser (Page(..))

data Frontmatter = Frontmatter
  { space    :: !Text
  , property :: !Text
  , value    :: !Bool
  , proof    :: !(Maybe Assumptions)
  } deriving Generic

instance ToJSON Assumptions where
  toJSON Assumptions{..} = object
    [ "traits"   .= S.toList assumedTraits
    , "theorems" .= (map unTheoremId $ S.toList assumedTheorems)
    ]

instance FromJSON Assumptions where
  parseJSON = withObject "Assumptions" $ \o -> Assumptions
    <$> o .: "traits"
    <*> o .: "theorems"

instance ToJSON Frontmatter
instance FromJSON Frontmatter

path :: Trait Space Property -> TreeFilePath
path Trait{..} = encodeUtf8 $ "spaces/" <> (unSpaceId $ spaceId traitSpace) <> "/properties/" <> (unPropertyId $ propertyId traitProperty) <> ".md"

parse :: Page Frontmatter -> Either Error (Trait Text Text, Maybe Assumptions)
parse (Page _ Frontmatter{..} main sections) = do
  let trait = Trait space property value main
  case HM.lookup "Proof" $ HM.fromList sections of
    Nothing -> return (trait, Nothing)
    Just p -> do
      pids <- parseProof p
      return (trait, Just pids)

parser = Page.Trait.parse

write :: (Trait Space Property, Maybe Assumptions) -> Page Frontmatter
write (t, proof) = Page
  { pagePath = path t
  , pageFrontmatter = Frontmatter
    { space    = unSpaceId $ traitSpaceId t
    , property = unPropertyId $ traitPropertyId t
    , value    = traitValue t
    , proof    = proof
    }
  , pageMain = traitDescription t
  , pageSections = []
  }

data Assumption = AssumedTrait TraitId | AssumedTheorem TheoremId

parseProof :: Text -> Either Error Assumptions
parseProof text = case parseOnly (sepBy1 parseAssumption "\n") text of
  Left err -> Left $ ParseError "proof" err
  Right as -> Right $ foldAssumptions as

parseAssumption = do
  _   <- "* ["
  _id <- takeTill $ \c -> c == ']'
  _   <- takeTill $ \c -> c == '\n'
  return $ assumptionFromId _id

assumptionFromId = error "assumptionFromId"
-- assumptionFromId :: Text -> Assumption
-- assumptionFromId _id = case T.uncons _id of
--   Just ('T', _) -> AssumedTrait $ TraitId _id
--   _             -> AssumedTheorem $ TheoremId _id

foldAssumptions :: [Assumption] -> Assumptions
foldAssumptions = error "foldAssumptions"
