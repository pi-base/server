{-# LANGUAGE DeriveGeneric #-}
module Page.Theorem
  ( parse
  , write
  ) where

import Data.Aeson

import Core
import Formula (Formula)
import Page.Parser (Page(..))

data Frontmatter = Frontmatter
  { uid :: TheoremId
  , _if :: Formula Text
  , _then :: Formula Text
  , _converse :: Maybe [TheoremId]
  } deriving Generic

instance ToJSON Frontmatter
instance FromJSON Frontmatter

parse :: Page Frontmatter -> Either Error (Theorem Text)
parse (Page _ Frontmatter{..} main _sections) = Right $ Theorem
  { theoremId = uid
  , theoremIf = _if
  , theoremThen = _then
  , theoremConverse = _converse
  , theoremDescription = main
  }

write :: Theorem Text -> Page Frontmatter
write Theorem{..} = Page
  { pagePath = encodeUtf8 $ "theorems/" <> _id <> ".md"
  , pageFrontmatter = Frontmatter
    { uid  = theoremId
    , _if = theoremIf
    , _then = theoremThen
    , _converse = theoremConverse
    }
  , pageMain = theoremDescription
  , pageSections = []
  }
  where
    (TheoremId _id) = theoremId
