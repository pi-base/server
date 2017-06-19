{-# LANGUAGE DeriveGeneric #-}
module Page.Trait
  ( page
  ) where

import Data.Aeson
import Data.Aeson.Types (Parser)
import qualified Data.HashMap.Strict as HM

import Core
import qualified Page

page :: Page (Trait SpaceId PropertyId)
page = Page.build write parse

path :: Trait SpaceId PropertyId -> TreeFilePath
path Trait{..} = encodeUtf8 $ "spaces/" <> (unSpaceId _traitSpace) <> "/properties/" <> (unPropertyId _traitProperty) <> ".md"

parse :: PageData -> Parser (Trait SpaceId PropertyId)
parse PageData{..} = do
  _traitSpace    <- pageFrontmatter .: "space"
  _traitProperty <- pageFrontmatter .: "property"
  _traitValue    <- pageFrontmatter .: "value"
  let _traitDescription = pageMain
  return Trait{..}

write :: Trait SpaceId PropertyId -> PageData
write t@Trait{..} = PageData
  { pagePath = path t
  , pageFrontmatter = HM.fromList
    [ "space"    .= _traitSpace
    , "property" .= _traitProperty
    , "value"    .= _traitValue
    ]
  , pageMain = _traitDescription
  , pageSections = mempty
  }
