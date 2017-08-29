module Data.Theorem
  ( describe
  , find
  , fetch
  , pending
  , put
  ) where

import Core                 hiding (find)
import Data                 (makeId, persistView, updateView, bridgeLoader, viewDeductions)
import qualified Data.Map   as M
import qualified Data.Parse
import qualified Data.Set   as S
import qualified Logic      as L
import Util                 (indexBy)

import Git (TreeT)

import Control.Monad.Trans.Except

describe :: (MonadStore m, MonadThrow m) => Maybe Committish -> Theorem p -> m Text
describe mc t = case mc of
  Nothing -> return $ theoremDescription t
  Just c  -> fmap theoremDescription . fetch c $ theoremId t

find :: MonadStore m => Committish -> TheoremId -> m (Maybe (Theorem Property))
find = Data.Parse.findTheorem

fetch :: (MonadStore m, MonadThrow m) => Committish -> TheoremId -> m (Theorem Property)
fetch sha _id = find sha _id >>= maybe (throwM . NotFound $ unTheoremId _id) return

pending :: TheoremId
pending = TheoremId ""

put :: (MonadStore m, MonadThrow m) => Ref -> CommitMeta -> Theorem PropertyId -> m (Either [Error] View)
put ref meta theorem' = do
  theorem <- assignId theorem'

  result <- updateView ref meta $ do
    cloader <- Data.Parse.cloader
    let loader = bridgeLoader cloader
    (lift $ L.runLogicT loader $ L.assertTheorem theorem) >>= \case
      Left err -> return . Left $ LogicError err
      Right updates -> lift $ viewDeductions cloader updates
  case result of
    Left err -> return $ Left [ err ]
    Right v  -> return $ Right v

assignId :: MonadIO m => Theorem p -> m (Theorem p)
assignId t = if theoremId t == pending
  then do
    _id <- makeId TheoremId "t"
    return $ t { theoremId = _id }
  else return t
