module Handler.Helpers
  ( attachToken
  , generateToken
  , maybeToken
  , requireToken
  ) where

import Import.NoFoundation

import qualified Data.Text as T
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID

generateToken :: (BaseBackend (YesodPersistBackend site) ~
                SqlBackend,
                PersistStoreWrite (YesodPersistBackend site), YesodPersist site)
              => UserId
              -> HandlerT site IO Token
generateToken userId = do
  uuid <- liftIO UUID.nextRandom
  (Entity _ token) <- attachToken userId $ UUID.toText uuid
  return token

maybeToken :: (BaseBackend (YesodPersistBackend site) ~ SqlBackend,
               PersistUniqueRead (YesodPersistBackend site),
               YesodPersist site)
           => HandlerT site IO (Maybe (Entity User))
maybeToken = lookupHeader "Authorization" >>= \case
  Nothing -> return Nothing
  Just header -> do
    mtoken <- runDB . getBy . UniqueToken $ T.replace "Bearer " "" (decodeUtf8 header)
    case mtoken of
      Nothing -> return Nothing
      Just (Entity _ token) -> do
        muser <- runDB . get $ tokenUserId token
        case muser of
          Nothing -> return Nothing
          Just user -> return . Just $ Entity (tokenUserId token) user

requireToken :: (BaseBackend (YesodPersistBackend site) ~ SqlBackend,
                 PersistUniqueRead (YesodPersistBackend site),
                 YesodPersist site)
             => HandlerT site IO (Entity User)
requireToken = maybeToken >>= \case
  Just eu -> return eu
  Nothing -> sendStatusJSON status401 $ object
    [ ("errors" :: Text) .= object
      [ "message" .= ("Invalid Authorization token" :: Text)
      , "type"    .= ("NOT_AUTHORIZED" :: Text)
      ]
    ]

attachToken :: ( BaseBackend (YesodPersistBackend site) ~ SqlBackend
               , PersistStoreWrite (YesodPersistBackend site)
               , YesodPersist site)
            => Key User -> Text -> HandlerT site IO (Entity Token)
attachToken userId token = do
  now <- liftIO getCurrentTime
  runDB $ insertEntity Token
    { tokenUserId    = userId
    , tokenIssuedAt  = now
    , tokenExpiredAt = Nothing
    , tokenUuid      = token
    }
