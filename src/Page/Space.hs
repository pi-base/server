module Page.Space
  ( page
  ) where

import Data.Aeson
import Data.Aeson.Types (Parser)
import qualified Data.HashMap.Strict as HM

import Core
import qualified Page

page :: Page Space
page = Page.build write parse

parse :: PageData -> Parser Space
parse PageData{..} = do
  spaceId      <- pageFrontmatter .: "uid"
  spaceSlug    <- pageFrontmatter .: "slug"
  spaceName    <- pageFrontmatter .: "name"
  spaceAliases <- pageFrontmatter .:? "aliases" .!= []
  let spaceDescription = pageMain
      spaceTopology = HM.lookup "Proof of Topology" $ pageSections
  return Space{..}

write :: Space -> PageData
write Space{..} = PageData
  { pagePath = encodeUtf8 $ "spaces/" <> spaceSlug <> "/README.md"
  , pageFrontmatter = HM.fromList
    [ "uid"  .= spaceId
    , "slug" .= spaceSlug
    , "name" .= spaceName
    ]
  , pageMain = spaceDescription
  , pageSections = HM.fromList $ case spaceTopology of
      Just top -> [("Proof of Topology", top)]
      Nothing  -> []
  }
