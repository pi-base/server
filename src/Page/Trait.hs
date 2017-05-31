{-# LANGUAGE DeriveGeneric #-}
module Page.Trait
  ( Page.Trait.parse
  , path
  , write
  ) where

import Data.Aeson
import Data.Attoparsec.Text hiding (space)
import Data.Either.Combinators (mapLeft)
import qualified Data.HashMap.Strict as HM
import qualified Data.Text           as T

import Core
import Page.Parser (Page(..))

data Frontmatter = Frontmatter
  { uid :: TraitId
  , space :: Text
  , property :: Text
  , value :: Bool
  } deriving Generic

instance ToJSON Frontmatter
instance FromJSON Frontmatter

path :: Trait Space Property -> TreeFilePath
path Trait{..} = encodeUtf8 $ "spaces/" <> (spaceSlug traitSpace) <> "/properties/" <> (propertySlug traitProperty) <> ".md"

parse :: Page Frontmatter -> Either Error (Trait Text Text, Maybe [Assumption])
parse (Page _ Frontmatter{..} main sections) = do
  let trait = Trait uid space property value main
  case HM.lookup "Proof" $ HM.fromList sections of
    Nothing -> return (trait, Nothing)
    Just p -> do
      pids <- parseProof p
      return (trait, Just pids)

write :: (Trait Space Property, Maybe [Assumption]) -> Page Frontmatter
write (t@Trait{..}, proof) = Page
  { pagePath = path t
  , pageFrontmatter = Frontmatter
    { uid = traitId
    , space = spaceSlug traitSpace
    , property = propertySlug traitProperty
    , value = traitValue
    }
  , pageMain = traitDescription
  , pageSections = case proof of
      Just assumptions -> error "assumptions"
      Nothing -> []
  }

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
