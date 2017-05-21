module Handler.Helpers
  ( createToken
  ) where

import Import.NoFoundation

import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID

createToken :: (BaseBackend (YesodPersistBackend site) ~
                SqlBackend,
                PersistStoreWrite (YesodPersistBackend site), YesodPersist site)
            => UserId
            -> HandlerT site IO Token
createToken userId = do
  now <- liftIO getCurrentTime
  uuid <- liftIO UUID.nextRandom

  let token = Token
        { tokenUserId = userId
        , tokenIssuedAt = now
        , tokenExpiredAt = Nothing
        , tokenUuid = UUID.toText uuid
        }
  _ <- runDB $ insert token
  return token
