module Page.Theorem
  ( page
  ) where

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
  theoremImplication <- fmap PropertyId <$>
    ( Implication
      <$> pageFrontmatter .: "if"
      <*> pageFrontmatter .: "then"
    )
  let theoremDescription = pageMain
  return Theorem{..}

write :: Theorem PropertyId -> PageData
write t@Theorem{..} = PageData
  { pagePath = encodeUtf8 $ "theorems/" <> unTheoremId theoremId <> ".md"
  , pageFrontmatter = HM.fromList
    [ "uid"      .= theoremId
    , "if"       .= (unPropertyId <$> theoremIf t)
    , "then"     .= (unPropertyId <$> theoremThen t)
    , "converse" .= theoremConverse
    ]
  , pageMain = theoremDescription
  , pageSections = mempty
  }
