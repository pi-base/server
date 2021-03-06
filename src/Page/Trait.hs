module Page.Trait
  ( page
  ) where

import Protolude

import Data.Aeson
import Data.Aeson.Types (Parser)
import qualified Data.HashMap.Strict as HM

import Core
import qualified Page

page :: Page (Trait SpaceId PropertyId)
page = Page.build write parse

path :: Trait SpaceId PropertyId -> TreeFilePath
path Trait{..} = encodeUtf8 $ "spaces/" <> (unId _traitSpace) <> "/properties/" <> (unId _traitProperty) <> ".md"

parse :: PageData -> Parser (Trait SpaceId PropertyId)
parse PageData{..} = do
  _traitSpace    <- pageFrontmatter .: "space"
  _traitProperty <- pageFrontmatter .: "property"
  _traitValue    <- pageFrontmatter .: "value"
  _traitRefs     <- pageFrontmatter .:? "refs" .!= []
  let _traitDescription = pageMain
  return Trait{..}

write :: Trait SpaceId PropertyId -> PageData
write t@Trait{..} = PageData
  { pagePath = path t
  , pageFrontmatter = HM.fromList
    [ "space"    .= _traitSpace
    , "property" .= _traitProperty
    , "value"    .= _traitValue
    , "refs"     .= _traitRefs
    ]
  , pageMain = _traitDescription
  , pageSections = mempty
  }
