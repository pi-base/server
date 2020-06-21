{-# LANGUAGE TemplateHaskell #-}
module Persist.Repo
  ( Repo
  , Action(..)
  , allBranches
  , createBranch
  , ignore
  , get
  , put
  , runIO
  , scan
  , version
  ) where

import Core hiding (Commit, Version, ignore)
import qualified Core

import qualified Data.Branch         as Branch
import qualified Data.User           as User
import           Persist.Auth        (Auth)
import qualified Persist.Auth        as Auth
import qualified Persist.Backend.Git as Git

data Action
  = Added   Text
  | Updated Text
  | Message Text

data Repo m a where
  AllBranches  :: Repo m [Branch]
  CreateBranch :: Branch -> Branch -> Repo m ()
  Get          :: Git.Pagable b => Git.Page b i -> Git.Paths i -> Branch -> i -> Repo m (Maybe (b Identity))
  Put          :: Git.Pagable b => Git.Page b i -> Git.Paths i -> Branch -> Action -> i -> (b Identity) -> Repo m ()
  Scan         :: Git.Pagable b => Git.Page b i -> Git.Paths i -> Branch -> Repo m [b Identity]
  Version      :: Branch -> Repo m Core.Version

makeSem ''Repo

ignore :: Sem (Repo ': r) a -> Sem r a
ignore = interpret \case
  AllBranches      -> return []
  CreateBranch _ _ -> return ()
  Get      _ _ _ _ -> return Nothing
  Put  _ _ _ _ _ _ -> return ()
  Scan       _ _ _ -> return []
  Version        b -> return $ Core.Version $ Branch._name b

runIO :: Members '[Auth, Embed IO] r
      => Git.Env
      -> Sem (Repo ': r) a
      -> Sem r a
runIO env = interpret \case
  AllBranches       -> Git.run env Git.branches
  CreateBranch  b f -> Git.run env $ Git.createBranch b f
  Get     pg pt b i -> Git.run env $ Git.find pg pt b i
  Put pg pt b a i v -> do
    u <- fromMaybe User.system <$> Auth.currentUser
    let c = Core.Commit u $ toMessage a
    Git.run env $ Git.write pg pt b c i v
  Scan      pg pt b -> Git.run env $ Git.scan pg pt b
  Version         b -> Git.run env $ Git.head b

toMessage :: Action -> Text
toMessage (Added   t) = "Add " <> t
toMessage (Updated t) = "Updated " <> t
toMessage (Message t) = t
