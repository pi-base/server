module Page.Property
  ( page
  ) where

import Data.Aeson
import Data.Aeson.Types (Parser)
import qualified Data.HashMap.Strict as HM

import Core
import qualified Page

page :: Page Property
page = Page.build write parse

parse :: PageData -> Parser Property
parse PageData{..} = do
  propertyId      <- pageFrontmatter .: "uid"
  propertySlug    <- pageFrontmatter .: "slug"
  propertyName    <- pageFrontmatter .: "name"
  propertyAliases <- pageFrontmatter .:? "aliases"
  let propertyDescription = pageMain
  return Property{..}

write :: Property -> PageData
write Property{..} = PageData
  { pagePath = encodeUtf8 $ "properties/" <> propertyName
  , pageFrontmatter = HM.fromList
    [ "uid"     .= propertyId
    , "slug"    .= propertySlug
    , "name"    .= propertyName
    , "aliases" .= propertyAliases
    ]
  , pageMain = propertyDescription
  , pageSections = mempty
  }
