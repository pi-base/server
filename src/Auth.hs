{-# LANGUAGE TemplateHaskell #-}
module Auth
  ( currentUser
  , ensureIdent
  , forUAT
  , generateToken
  , requireUser
  , userWithToken
  ) where

import Core

import           Data.Time        (getCurrentTime)
import qualified Data.UUID        as UUID
import qualified Data.UUID.V4     as UUID
import           Database.Persist

import           Util             (findOrCreate)

type Provider = Text

requireUser :: Auth m => m (Entity User)
requireUser = currentUser >>= \case
  Just user -> return user
  Nothing -> throwIO NotAuthenticated

userWithToken :: DB m => AuthToken -> m (Maybe (Entity User))
userWithToken cred = do
  mtoken <- db $ getBy $ UniqueToken cred
  case mtoken of
    Nothing -> return Nothing
    Just (Entity _ token) -> do
      muser <- db $ get $ tokenUserId token
      case muser of
        Nothing -> return Nothing
        Just user -> return . Just $ Entity (tokenUserId token) user

generateToken :: DB m => UserId -> m Token
generateToken userId = do
  uuid <- liftIO UUID.nextRandom
  (Entity _ token) <- attachToken userId $ UUID.toText uuid
  return token

attachToken :: DB m => Key User -> Text -> m (Entity Token)
attachToken userId token = do
  now <- liftIO getCurrentTime
  db $ insertEntity Token
    { tokenUserId    = userId
    , tokenIssuedAt  = now
    , tokenExpiredAt = Nothing
    , tokenUuid      = token
    }

ensureIdent :: (DB m, MonadLogger m)
            => Provider
            -> Text
            -> AuthToken
            -> User
            -> m UserId
ensureIdent provider uid accessToken user = do
  (Entity id _) <- db $ findOrCreate (UniqueUser . userEmail) user
  let ident = Ident
                { identUserId      = id
                , identProvider    = provider
                , identUid         = uid
                , identAccessToken = accessToken
                }
  void $ db $ findOrCreate (\Ident{..} -> UniqueIdent identProvider identUid) ident
  return id

forUAT :: (DB m, MonadLogger m) => m (User, Token)
forUAT = do
  let uat = User
              { userName       = "UAT"
              , userEmail      = "uat@uat.pi-base.org"
              , userIsReviewer = False
              }
  uuid   <- UUID.toText <$> liftIO UUID.nextRandom
  userId <- ensureIdent "test" "uat" uuid uat
  token  <- generateToken userId
  return (uat, token)
