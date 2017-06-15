module Handler.Graph (postGraphR) where

import Import
import qualified Graph.Root

postGraphR :: Handler Value
postGraphR = do
  body <- requireJsonBody
  -- putStrLn $ "[GraphQL] " <> tshow body
  Graph.Root.query body
  -- $(logDebug) $ "[GraphQL] " <> (toStrict . decodeUtf8 $ encode value)
