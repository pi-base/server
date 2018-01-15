{-# LANGUAGE TemplateHaskell #-}
module Handler.Helpers
  ( attachToken
  , ensureToken
  , ensureUser
  , generateToken
  , maybeToken
  , requireToken
  , rollbar
  ) where

import Import.NoFoundation
import Class

import qualified Data.Text as T
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID

import Rollbar

import Data.Helpers (findOrCreate)

generateToken :: MonadDB m => UserId -> m Token
generateToken userId = do
  uuid <- liftIO UUID.nextRandom
  (Entity _ token) <- attachToken userId $ UUID.toText uuid
  return token

maybeToken :: (MonadDB m, MonadHandler m) => m (Maybe (Entity User))
maybeToken = lookupHeader "Authorization" >>= \case
  Nothing -> return Nothing
  Just header -> do
    mtoken <- db . getBy . UniqueToken $ T.replace "Bearer " "" (decodeUtf8 header)
    case mtoken of
      Nothing -> return Nothing
      Just (Entity _ token) -> do
        muser <- db . get $ tokenUserId token
        case muser of
          Nothing -> return Nothing
          Just user -> return . Just $ Entity (tokenUserId token) user

requireToken :: (MonadDB m, MonadHandler m) => m (Entity User)
requireToken = maybeToken >>= \case
  Just eu -> return eu
  Nothing -> sendStatusJSON status401 $ object
    [ ("errors" :: Text) .= object
      [ "message" .= ("Invalid Authorization token" :: Text)
      , "type"    .= ("NOT_AUTHORIZED" :: Text)
      ]
    ]

attachToken :: MonadDB m => Key User -> Text -> m (Entity Token)
attachToken userId token = do
  now <- liftIO getCurrentTime
  db $ insertEntity Token
    { tokenUserId    = userId
    , tokenIssuedAt  = now
    , tokenExpiredAt = Nothing
    , tokenUuid      = token
    }

ensureUser :: MonadDB m => User -> m (Entity User)
ensureUser user = findOrCreate (UniqueUser . userIdent) $ user

ensureToken :: MonadDB m => UserId -> Text -> m (Entity Token)
ensureToken _id token = do
  now <- liftIO getCurrentTime
  findOrCreate (UniqueToken . tokenUuid) $ Token _id now Nothing token

rollbar :: (MonadIO m, MonadBaseControl IO m, Show e) 
        => AppSettings -> Maybe (Entity User) -> e -> m ()
rollbar settings muser e = do
  let rollbarPerson (Entity uid user) =
         Rollbar.Person
           { Rollbar.id       = toPathPiece uid
           , Rollbar.username = Nothing
           , Rollbar.email    = Just $ userEmail user
           }
      options = Rollbar.Options (rollbarPerson <$> muser) (Just $ appBuild settings)
  reportErrorS (appRollbar settings)
               options
               "errorHandler"
               (tshow e)