{-# LANGUAGE TemplateHaskell #-}
module Interpreter
  ( AuthConfig(..)
  , Config(..)
  , Env
  , Ex(..)
  , HttpConfig(..)
  , StoreConfig(..)
  , auth
  , boot
  , gatherErrors
  , io
  , initial
  , http
  , master
  , memory
  , pure
  , run
  , serve
  , status
  , store
  ) where

import Server.Import hiding (pure, serve)
import Server.Api    (NotFound)

import qualified Persist.Auth        as Auth
import           Persist.Auth        (Forbidden(..), LoginRequired(..))
import qualified Persist.Backend.Git as Git
import qualified Persist.Branches    as Branches
import qualified Persist.DB          as DB
import qualified Persist.Github      as Github
import qualified Persist.Http        as Http
import qualified Persist.Properties  as Properties
import qualified Persist.Repo        as Repo
import qualified Persist.Spaces      as Spaces
import qualified Persist.Store       as Store
import qualified Persist.Theorems    as Theorems
import qualified Persist.Traits      as Traits
import           Polysemy.Error      (Error(..), mapError, runError)
import           Polysemy.Reader     (Reader, runReader)
import           Polysemy            (embed, interpretH, runM)
import           Polysemy.State      (runStateIORef)

data Ex
  = ForbiddenError     Forbidden
  | LoginRequiredError LoginRequired
  | NotFoundError      NotFound
  deriving (Show, Eq)

data State = State
  { auths      :: IORef Auth.State
  , properties :: IORef (Store.State PropertyId Property)
  , spaces     :: IORef (Store.State SpaceId    Space)
  , theorems   :: IORef (Store.State TheoremId  Theorem)
  , traits     :: IORef (Store.State TraitId    Trait)
  , branches   :: IORef Branches.State
  , github     :: IORef Github.State
  }

data StoreConfig
  = StoreIO DB.Config Git.Config
  | StoreMemory
  | StorePure
  deriving (Generic, Show, Eq)

data HttpConfig
  = HttpLive Github.Config
  | HttpPure
  deriving (Generic, Show, Eq)

data AuthConfig
  = AuthMemory
  | AuthPure
  deriving (Generic, Show, Eq)

data Config = Config
  { _master :: Branch
  , _store  :: StoreConfig
  , _http   :: HttpConfig
  , _auth   :: AuthConfig
  , _status :: Status
  } deriving (Generic, Show, Eq)

data StoreE
  = StoreIO' DB.Env Git.Env
  | StoreMemory' State
  | StorePure'

data HttpE
  = HttpLive' Github.Config
  | HttpPure'

data AuthE
  = AuthMemory' (IORef Auth.State)
  | AuthPure'

data Env = Env
  { _master' :: Branch
  , _store'  :: StoreE
  , _http'   :: HttpE
  , _auth'   :: AuthE
  , _status' :: Status
  }

makeLenses ''Config
makeLenses ''Env

boot :: Config -> IO Env
boot c = Env
  <$> return (c ^. master)
  <*> case c ^. store of
    StoreIO db git -> StoreIO'
      <$> DB.initialize db
      <*> Git.initialize git
    StoreMemory -> StoreMemory' <$> initial
    StorePure -> return StorePure'
  <*> case c ^. http of
    HttpLive gh -> return $ HttpLive' gh
    HttpPure -> return HttpPure'
  <*> case c ^. auth of
    AuthMemory -> AuthMemory' <$> newIORef Auth.initial
    AuthPure -> return AuthPure'
  <*> return (c ^. status)

pure :: Branch -> Status -> Config
pure base = Config base StorePure HttpPure AuthPure

memory :: Branch -> Status -> Config
memory base = Config base StoreMemory HttpPure AuthMemory

io :: Branch -> Status -> Config
io base = Config base (StoreIO db git) HttpPure AuthMemory
  where
    db = DB.Config "postgresql://localhost/pi_base_dev" 5
    git = Git.Config "/data/src/pibase/server/tmp/repo.git"

run :: Member (Embed IO) r
  => Env
  -> Sem
     ( Properties
    ': Spaces
    ': Theorems
    ': Traits
    ': Branches
    ': Repo
    ': DB
    ': Github
    ': Http
    ': Auth
    ': Reader Status
    ': r
     ) a
  -> Sem r a
run settings action = action
  & runStore  (settings ^. master') (settings ^. store')
  & runHttp   (settings ^. http')
  & runAuth   (settings ^. auth')
  & runReader (settings ^. status')

gatherErrors ::
    Sem
     ( Error Auth.Forbidden
    ': Error Auth.LoginRequired
    ': Error NotFound
    ': Error Ex
    ': r
     ) a
  -> Sem r (Either Ex a)
gatherErrors action = action
  & mapError ForbiddenError
  & mapError LoginRequiredError
  & mapError NotFoundError
  & runError

serve :: MonadIO m =>
  Sem
    '[ Error LoginRequired
     , Error Forbidden
     , Error NotFound
     , Error ServerError
     , Embed IO
     ] a
  -> m a
serve action = action
  & mapError (const err401)
  & mapError (const err403)
  & mapError (const err404)
  & errorIO
  & runM
  & liftIO

errorIO :: (Member (Embed IO) r, Exception e)
        => Sem (Error e ': r) a
        -> Sem r a
errorIO = interpretH \case
  Catch _ _ -> error "errorIO" -- TODO
  Throw   e -> embed $ throwIO e

initial :: IO State
initial = State
  <$> newIORef Auth.initial
  <*> newIORef Store.initial
  <*> newIORef Store.initial
  <*> newIORef Store.initial
  <*> newIORef Store.initial
  <*> newIORef Branches.initial
  <*> newIORef Github.initial

runStore ::
     Members '[Auth, Github, Embed IO] r
  => Branch
  -> StoreE
  -> Sem
     ( Properties
    ': Spaces
    ': Theorems
    ': Traits
    ': Branches
    ': Repo
    ': DB
    ': r
     ) a
  -> Sem r a
runStore base (StoreIO' db git) action = action
  & Properties.runRepo
  & Spaces.runRepo
  & Theorems.runRepo
  & Traits.runRepo
  & Branches.runIO base
  & Repo.runIO git
  & DB.runIO db
runStore base (StoreMemory' State{..}) action = action
  & Properties.toState & runStateIORef properties
  & Spaces.toState     & runStateIORef spaces
  & Theorems.toState   & runStateIORef theorems
  & Traits.toState     & runStateIORef traits
  & Branches.toState base & runStateIORef branches
  & Repo.ignore -- TODO: log commits?
  & DB.disallowed
runStore base StorePure' action = action
  & Properties.runState []
  & Spaces.runState     []
  & Theorems.runState   []
  & Traits.runState     []
  & Branches.runState base
  & Repo.ignore
  & DB.disallowed

runHttp :: Member (Embed IO) r
  => HttpE
  -> Sem
     ( Github
    ': Http
    ': r
     ) a
  -> Sem r a
runHttp (HttpLive' github) action = action
  & Github.runHttp github
  & Http.runWreq
runHttp HttpPure' action = action
  & Github.runState
  & Http.runList []

runAuth :: Member (Embed IO) r
  => AuthE
  -> Sem (Auth ': r) a
  -> Sem r a
runAuth (AuthMemory' ref) action = action
  & Auth.toState & runStateIORef ref
runAuth AuthPure' action = action
  & Auth.evalState
