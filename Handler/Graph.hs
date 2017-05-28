module Handler.Graph (postGraphR) where

import Import
import qualified Graph.Root

postGraphR :: Handler Value
postGraphR = do
  value <- requireJsonBody >>= Graph.Root.query
  -- $(logDebug) $ "[GraphQL] " <> (toStrict . decodeUtf8 $ encode value)
  return value
