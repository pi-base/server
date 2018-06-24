module Page.Theorem
  ( page
  ) where

import Protolude

import Data.Aeson
import Data.Aeson.Types (Parser)
import qualified Data.HashMap.Strict as HM

import Core
import qualified Page

page :: Page (Theorem PropertyId)
page = Page.build write parse

parse :: PageData -> Parser (Theorem PropertyId)
parse PageData{..} = do
  theoremId          <- pageFrontmatter .: "uid"
  theoremConverse    <- pageFrontmatter .:? "converse"
  theoremImplication <- fmap Id <$>
    ( Implication
      <$> pageFrontmatter .: "if"
      <*> pageFrontmatter .: "then"
    )
  theoremRefs <- pageFrontmatter .:? "refs" .!= []
  let theoremDescription = pageMain
  return Theorem{..}

write :: Theorem PropertyId -> PageData
write t@Theorem{..} = PageData
  { pagePath = encodeUtf8 $ "theorems/" <> unId theoremId <> ".md"
  , pageFrontmatter = HM.fromList
    [ "uid"      .= theoremId
    , "if"       .= (unId <$> theoremIf t)
    , "then"     .= (unId <$> theoremThen t)
    , "converse" .= theoremConverse
    , "refs"     .= theoremRefs
    ]
  , pageMain = theoremDescription
  , pageSections = mempty
  }
