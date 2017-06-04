{-# LANGUAGE DeriveGeneric #-}
module Page.Theorem
  ( parser
  , parse
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

instance ToJSON Frontmatter where
  toJSON Frontmatter{..} = object
    [ "uid" .= uid
    , "if" .= _if
    , "then" .= _then
    , "converse" .= _converse
    ]
instance FromJSON Frontmatter where
  parseJSON = withObject "Theorem Frontmatter" $ \o -> Frontmatter
    <$> o .: "uid"
    <*> o .: "if"
    <*> o .: "then"
    <*> o .:? "converse"

parser = parse

parse :: Page Frontmatter -> Either Error (Theorem Text)
parse (Page _ Frontmatter{..} main _sections) = Right $ Theorem
  { theoremId = uid
  , theoremIf = _if
  , theoremThen = _then
  , theoremConverse = _converse
  , theoremDescription = main
  }

write :: Theorem Property -> Page Frontmatter
write Theorem{..} = Page
  { pagePath = encodeUtf8 $ "theorems/" <> unTheoremId theoremId <> ".md"
  , pageFrontmatter = Frontmatter
    { uid = theoremId
    , _if = serialize theoremIf
    , _then = serialize theoremThen
    , _converse = theoremConverse
    }
  , pageMain = theoremDescription
  , pageSections = []
  }
  where
    serialize :: Formula Property -> Formula Text
    serialize = map (unPropertyId . propertyId)
