module Page.Property
  ( page
  ) where

import Protolude
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
  propertyAliases <- pageFrontmatter .:? "aliases" .!= []
  propertyRefs    <- pageFrontmatter .:? "refs" .!= []
  let propertyDescription = pageMain
  return Property{..}

write :: Property -> PageData
write Property{..} = PageData
  { pagePath = encodeUtf8 $ "properties/" <> unId propertyId <> ".md"
  , pageFrontmatter = HM.fromList
    [ "uid"     .= propertyId
    , "slug"    .= propertySlug
    , "name"    .= propertyName
    , "aliases" .= propertyAliases
    , "refs"    .= propertyRefs
    ]
  , pageMain = propertyDescription
  , pageSections = mempty
  }
