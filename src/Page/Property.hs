{-# LANGUAGE DeriveGeneric #-}
module Page.Property
  ( Frontmatter(..)
  , parser
  , parse
  , write
  ) where

import Data.Aeson

import Core
import Page.Parser (Page(..))

data Frontmatter = Frontmatter
  { uid :: PropertyId
  , slug :: Text
  , name :: Text
  , aliases :: Maybe [Text]
  } deriving Generic

instance ToJSON Frontmatter
instance FromJSON Frontmatter

parser :: Page Frontmatter -> Either Error Property
parser = parse

parse :: Page Frontmatter -> Either Error Property
parse (Page _ Frontmatter{..} main _sections) = Right $ Property
  { propertyId = uid
  , propertySlug = slug
  , propertyName = name
  , propertyAliases = aliases
  , propertyDescription = main
  }

write :: Property -> Page Frontmatter
write Property{..} = Page
  { pagePath = encodeUtf8 $ "properties/" <> propertySlug <> ".md"
  , pageFrontmatter = Frontmatter
    { uid  = propertyId
    , slug = propertySlug
    , name = propertyName
    , aliases = propertyAliases
    }
  , pageMain = propertyDescription
  , pageSections = []
  }
