{-# LANGUAGE TemplateHaskell #-}
module Persist.Auth
  ( Access(..)
  , Auth
  , Forbidden(..)
  , LoginRequired(..)
  , State
  , check
  , create
  , currentUser
  , evalState
  , forToken
  , initial
  , grant
  , login
  , requireAccess
  , requireUser
  , toState
  ) where

import Core

import           Control.Lens
import qualified Data.Branch    as Branch
import qualified Persist.DB     as DB
import           Polysemy.Error (Error, throw)
import qualified Polysemy.State as S

data LoginRequired = LoginRequired
  deriving (Show, Eq)

data Forbidden = Forbidden
  deriving (Show, Eq)

data Auth m a where
  Check       :: User -> Branch -> Auth m Access
  Create      :: User -> Auth m ()
  CurrentUser :: Auth m (Maybe User)
  ForToken    :: Text -> Auth m (Maybe User)
  Grant       :: User -> Branch -> Access -> Auth m ()
  Login       :: User -> Auth m ()

makeSem ''Auth

data State = State
  { _sessionUser :: Maybe User
  , _grants      :: Map (User, Branch) Access
  }

makeLenses ''State

toState :: Sem (Auth ': r) a -> Sem (S.State State ': r) a
toState = reinterpret $ \case
  Check        user branch -> S.gets $ view $ grants . at (user, branch) . non None
  Create                 _ -> return ()
  CurrentUser              -> S.gets $ view sessionUser
  ForToken           token -> return $ Just $ User token token False
  Grant user branch access -> S.modify $ grants . at (user, branch) .~ Just access
  Login               user -> S.modify $ sessionUser .~ Just user

initial :: State
initial = State Nothing mempty

evalState :: Sem (Auth ': r) a -> Sem r a
evalState = S.evalState initial . toState

requireUser :: Members '[Auth, Error LoginRequired] r => Sem r User
requireUser = maybe (throw LoginRequired) return =<< currentUser

requireAccess :: Members '[Auth, Error Forbidden, Error LoginRequired] r
              => Branch
              -> Access
              -> Sem r ()
requireAccess branch required = do
  user   <- requireUser
  actual <- check user branch
  when (actual < required) $ throw Forbidden

toDB :: Members '[DB.DB, Embed IO] r
     => Sem (Auth ': r) a -> Sem (S.State (Maybe User) ': r) a
toDB = reinterpret \case
  Check        user branch -> DB.checkBranchAccess user (Branch._name branch)
  Create user              -> DB.createUser user
  CurrentUser              -> S.get
  ForToken           token -> DB.userForToken token
  Grant user branch access -> DB.grantBranchAccess user (Branch._name branch) access
  Login               user -> S.put $ Just user
