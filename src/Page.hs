module Page
  ( Parser
  , parse
  ) where

import Core
import qualified Page.Parser as P

type Parser f a = P.Page f -> Either Error a

parse :: FromJSON f
      => (Parser f a) -> TreeFilePath -> Text -> Either Error a
parse parser path contents = do
  page <- P.parse (path, contents)
  parser page
