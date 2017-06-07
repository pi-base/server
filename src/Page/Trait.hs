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
  , proof    :: !(Maybe Proof)
  } deriving Generic

instance ToJSON Frontmatter where
  toJSON Frontmatter{..} = object $
    [ "space"    .= space
    , "property" .= property
    , "value"    .= value
    ]
    -- Don't embed a `proof` field if there is no proof to show
    <> maybe [] (\p -> ["proof" .= p]) proof

instance ToJSON Proof where
  toJSON Proof{..} = object
    [ "properties" .= (map unPropertyId $ S.toList proofProperties)
    , "theorems"   .= (map unTheoremId  $ S.toList proofTheorems)
    ]

instance FromJSON Frontmatter where
  parseJSON = withObject "Frontmatter" $ \o -> do
    space    <- o .: "space"
    property <- o .: "property"
    value    <- o .: "value"
    let proof = error "parse proof from frontmatter"
    return Frontmatter{..}

path :: Trait Space Property -> TreeFilePath
path Trait{..} = encodeUtf8 $ "spaces/" <> (unSpaceId $ spaceId traitSpace) <> "/properties/" <> (unPropertyId $ propertyId traitProperty) <> ".md"

parse :: Page Frontmatter -> Either Error (Trait Text Text, Maybe Proof)
parse (Page _ Frontmatter{..} main _) =
  return (Trait space property value main, proof)

parser = Page.Trait.parse

write :: (Trait Space Property, Maybe Proof) -> Page Frontmatter
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
