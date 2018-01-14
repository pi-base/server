{-# LANGUAGE TemplateHaskell #-}
module Data.Theorem
  ( fetch
  , pending
  , put
  ) where

import           Core  hiding (find)
import           Data  (findParsed, makeId, updateView, viewDeductions)
import qualified Data.Parse as Parse
import qualified Data.Property
import qualified Logic as L

find :: MonadStore m => Branch -> TheoremId -> m (Maybe (Theorem Property))
find branch _id = findParsed Parse.theorem branch _id >>= \case
  Nothing -> return Nothing
  Just theorem ->
    mapM (Data.Property.find branch) theorem >>= return . sequence

fetch :: (MonadStore m, MonadThrow m) => Branch -> TheoremId -> m (Theorem Property)
fetch branch _id = find branch _id >>= maybe (throwM . NotFound $ unId _id) return

pending :: TheoremId
pending = Id ""

put :: (MonadStore m, MonadLogger m) 
    => Branch 
    -> CommitMeta 
    -> Theorem PropertyId 
    -> m View
put branch meta theorem' = do
  theorem <- assignId theorem'
  updateView branch meta $ \loader -> do
    $(logDebug) $ "Asserting " <> tshow (theoremImplication theorem)
    result <- L.runLogicT loader $ L.assertTheorem theorem
    $(logDebug) $ "Assertion yielded: " <> tshow result
    case result of
      Left      err -> throw $ LogicError err
      Right updates -> do
        view <- viewDeductions loader updates
        return view

assignId :: MonadIO m => Theorem p -> m (Theorem p)
assignId t = if theoremId t == pending
  then do
    _id <- makeId "t"
    return $ t { theoremId = _id }
  else return t
