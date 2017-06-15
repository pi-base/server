{-# LANGUAGE DeriveGeneric #-}
module Page.Space
  ( Frontmatter(..)
  , parse
  , parser
  , write
  ) where

import Data.Aeson
import qualified Data.HashMap.Strict as HM

import Core
import Page.Parser (Page(..))

data Frontmatter = Frontmatter
  { uid  :: SpaceId
  , slug :: Text
  , name :: Text
  } deriving Generic

instance ToJSON Frontmatter
instance FromJSON Frontmatter

parser :: Page Frontmatter -> Either Error Space
parser = parse

parse :: Page Frontmatter -> Either Error Space
parse (Page _ Frontmatter{..} main sections) = Right $ Space
  { spaceId = uid
  , spaceSlug = slug
  , spaceName = name
  , spaceDescription = main
  , spaceTopology = HM.lookup "Proof of Topology" $ HM.fromList sections
  }

write :: Space -> Page Frontmatter
write Space{..} = Page
  { pagePath = encodeUtf8 $ "spaces/" <> spaceSlug <> "/README.md"
  , pageFrontmatter = Frontmatter
    { uid  = spaceId
    , slug = spaceSlug
    , name = spaceName
    }
  , pageMain = spaceDescription
  , pageSections = case spaceTopology of
      Just top -> [("Proof of Topology", top)]
      Nothing  -> []
  }
