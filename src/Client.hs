{-# LANGUAGE TemplateHaskell #-}
module Client
  ( Client
  , branches
  , createProperty
  , createSpace
  , createTheorem
  , createTrait
  , initialize
  , me
  , root
  , runClient
  , status
  , submitBranch
  , updateProperty
  , updateSpace
  , updateTheorem
  , updateTrait
  , viewBranch
  ) where

import Server.Import hiding (Request, Status, addHeader)

import qualified Data.Branch         as Branch
import           Network.HTTP.Client (Manager, newManager, defaultManagerSettings)
import           Polysemy            (embed)
import           Servant.Client      (BaseUrl(BaseUrl), ClientM, Scheme(Http), client, mkClientEnv, runClientM)
import           Servant.Client.Core (AuthClientData, addHeader, mkAuthenticatedRequest)
import           Server.Api          (API)
import           Server.Class        ()
import qualified Server.Status       as Server

type instance AuthClientData (AuthProtect "token-auth") = Text

data Client m a where
  Root           :: Client m View
  Branches       :: Client m [Branch]
  ViewBranch     :: Branch.Name -> Client m View
  SubmitBranch   :: Branch.Name -> Client m (Either PullRequestError PullRequest)
  CreateSpace    :: Branch.Name -> CreateSpaceBody -> Client m Space
  UpdateSpace    :: Branch.Name -> SpaceId -> UpdateSpaceBody -> Client m Space
  CreateTrait    :: Branch.Name -> SpaceId -> PropertyId -> CreateTraitBody -> Client m Trait
  UpdateTrait    :: Branch.Name -> SpaceId -> PropertyId -> UpdateTraitBody -> Client m Trait
  CreateProperty :: Branch.Name -> CreatePropertyBody -> Client m Property
  UpdateProperty :: Branch.Name -> PropertyId -> UpdatePropertyBody -> Client m Property
  CreateTheorem  :: Branch.Name -> CreateTheoremBody -> Client m Theorem
  UpdateTheorem  :: Branch.Name -> TheoremId -> UpdateTheoremBody -> Client m Theorem
  Me             :: Client m User
  Status         :: Client m Server.Status

makeSem ''Client

data Commands m = Commands
  { _root           :: m View
  , _branches       :: m [Branch]
  , _viewBranch     :: Branch.Name -> m View
  , _submitBranch   :: Branch.Name -> m (Either PullRequestError PullRequest)
  , _createSpace    :: Branch.Name -> CreateSpaceBody -> m Space
  , _updateSpace    :: Branch.Name -> SpaceId -> UpdateSpaceBody -> m Space
  , _createTrait    :: Branch.Name -> SpaceId -> PropertyId -> CreateTraitBody -> m Trait
  , _updateTrait    :: Branch.Name -> SpaceId -> PropertyId -> UpdateTraitBody -> m Trait
  , _createProperty :: Branch.Name -> CreatePropertyBody -> m Property
  , _updateProperty :: Branch.Name -> PropertyId -> UpdatePropertyBody -> m Property
  , _createTheorem  :: Branch.Name -> CreateTheoremBody -> m Theorem
  , _updateTheorem  :: Branch.Name -> TheoremId -> UpdateTheoremBody -> m Theorem
  , _me             :: m User
  , _status         :: m Server.Status
  }

data State = State
  { _port     :: Int
  , _manager  :: Manager
  , _token    :: IORef Text
  , _commands :: IORef (Commands ClientM)
  }

initialize :: Int -> Text -> IO State
initialize port token = State
  <$> pure port
  <*> newManager defaultManagerSettings
  <*> newIORef token
  <*> newIORef (commands token)

exec :: State -> ClientM a -> IO a
exec State{..} action = do
  let url = BaseUrl Http "localhost" _port ""
  runClientM action (mkClientEnv _manager url) >>= either throwIO return

runClient :: Member (Embed IO) r
  => State
  -> Sem (Client ': r) a
  -> Sem r a
runClient state = interpret \case
  Root                 -> f _root identity
  Branches             -> f _branches identity
  ViewBranch         b -> f _viewBranch \g -> g b
  SubmitBranch       b -> f _submitBranch \g -> g b
  CreateSpace      b s -> f _createSpace \g -> g b s
  UpdateSpace    b i s -> f _updateSpace \g -> g b i s
  CreateTrait  b s p t -> f _createTrait \g -> g b s p t
  UpdateTrait  b s p t -> f _updateTrait \g -> g b s p t
  CreateProperty   b p -> f _createProperty \g -> g b p
  UpdateProperty b i p -> f _updateProperty \g -> g b i p
  CreateTheorem    b t -> f _createTheorem \g -> g b t
  UpdateTheorem  b i t -> f _updateTheorem \g -> g b i t
  Me                   -> f _me identity
  Status               -> f _status identity
  where
    f :: Member (Embed IO) r
      => (Commands ClientM -> b)
      -> (b -> ClientM a)
      -> Sem r a
    f field go = embed $ do
      action <- fmap field $ readIORef $ _commands state
      exec state $ go action

commands :: Text -> (Commands ClientM)
commands token =
  let
    req = mkAuthenticatedRequest token (addHeader "Authorization")
    _root
      :<|> _branches
      :<|> _viewBranch
      :<|> _submitBranch
      :<|> _createSpace
      :<|> _updateSpace
      :<|> _createTrait
      :<|> _updateTrait
      :<|> _createProperty
      :<|> _updateProperty
      :<|> _createTheorem
      :<|> _updateTheorem
      :<|> _me
      :<|> _status
      = client api req
  in
    Commands
      { _root
      , _branches
      , _viewBranch
      , _submitBranch
      , _createSpace
      , _updateSpace
      , _createTrait
      , _updateTrait
      , _createProperty
      , _updateProperty
      , _createTheorem
      , _updateTheorem
      , _me
      , _status
      }

api :: Proxy API
api = Proxy