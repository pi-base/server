{-# LANGUAGE ExplicitForAll      #-} 
{-# LANGUAGE ScopedTypeVariables #-} 
{-# LANGUAGE TemplateHaskell     #-} 
{-# LANGUAGE TypeApplications    #-} 
module Data.Branch.Move
  ( move
  , properties
  , spaces
  , theorems
  ) where

import Import
import Core

import qualified Data.Id   as Id
import qualified Data.Map  as M
import qualified Data.Text as T

move :: BranchName -> Map Uid Uid -> Handler ()
move branch mapping = do
  $(logDebug) $ T.intercalate ""
    [ "Moving files on "
    , branch
    , ": "
    , T.intercalate ", "
      ( map (\(k,v) -> k <> " => " <> v) $ M.toList mapping 
      )
    ]
  properties $ extract mapping
  spaces     $ extract mapping
  -- moveTheorems $ extract mapping

properties :: Map PropertyId PropertyId -> Handler ()
properties mapping = do
  $(logDebug) $ "props: " <> tshow mapping

spaces :: Map SpaceId SpaceId -> Handler ()
spaces mapping = do
  $(logDebug) $ "spaces: " <> tshow mapping

theorems :: Map TheoremId TheoremId -> Handler ()
theorems mapping = do
  $(logDebug) $ "theorems: " <> tshow mapping

extract :: Id.Identifiable a => Map Uid Uid -> Map (Id a) (Id a)
extract = foldr add M.empty . M.toList 
  where
    add :: forall a. Id.Identifiable a 
        => (Uid, Uid) -> Map (Id a) (Id a) -> Map (Id a) (Id a)
    add (k,v) = if (singleton $ Id.prefix @a) `T.isPrefixOf` v
      then M.insert (Id k) (Id v)
      else id