{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Viewer
  ( Viewer(..)
  , Proofs(..)
  , describe
  , empty
  ) where

import Core    hiding (empty)
import Formula hiding ((.=))

import           Data.Aeson
import qualified Data.Map   as M

newtype Proofs = Proofs (Map TraitId [Assumption]) deriving Show

data Viewer = Viewer
  { viewerProperties :: [Property]
  , viewerSpaces     :: [Space]
  , viewerTheorems   :: [Theorem Property]
  , viewerTraits     :: [Trait Space Property]
  , viewerProofs     :: Proofs
  , viewerVersion    :: Text
  }
  deriving Show

describe :: Viewer -> IO ()
describe Viewer{..} = do
  putStrLn $ "Version " <> viewerVersion

  putStrLn $ tshow (length viewerSpaces) <> " spaces"
  -- forM_ viewerSpaces print

  putStrLn $ tshow (length viewerProperties) <> " properties"
  -- forM_ viewerProperties print

  putStrLn $ tshow (length viewerTheorems) <> " theorems"
  -- forM_ viewerTheorems print

  putStrLn $ tshow (length viewerTraits) <> " traits"
  -- forM_ viewerTraits print

  let Proofs proofs = viewerProofs
  putStrLn $ tshow (length proofs) <> " proofs"
  -- forM_ (take 3 $ M.toList viewerProofs) print

empty :: Viewer
empty = Viewer
  { viewerSpaces = []
  , viewerProperties = []
  , viewerTraits = []
  , viewerTheorems = []
  , viewerProofs = Proofs M.empty
  , viewerVersion = ""
  }

instance ToJSON (Formula Property) where
  toJSON (Conj subs) = object [ "and" .= toJSON subs ]
  toJSON (Disj subs) = object [ "or"  .= toJSON subs ]
  toJSON (Atom p v)  = object [ _id   .= v ]
    where (PropertyId _id) = propertyId p

instance ToJSON Space where
  toJSON Space{..} = object
    [ "uid"               .= spaceId
    , "name"              .= spaceName
    , "slug"              .= spaceSlug
    , "description"       .= spaceDescription
    , "proof_of_topology" .= spaceTopology
    ]

instance ToJSON Property where
  toJSON Property{..} = object
    [ "uid"         .= propertyId
    , "name"        .= propertyName
    , "slug"        .= propertySlug
    , "aliases"     .= propertyAliases
    , "description" .= propertyDescription
    ]

instance ToJSON (Theorem Property) where
  toJSON Theorem{..} = object
    [ "uid"         .= theoremId
    , "if"          .= theoremIf
    , "then"        .= theoremThen
    , "converse"    .= theoremConverse
    , "description" .= theoremDescription
    ]

instance ToJSON (Trait Space Property) where
  toJSON Trait{..} = object
    [ "uid"         .= traitId
    , "space"       .= spaceId traitSpace
    , "property"    .= propertyId traitProperty
    , "value"       .= traitValue
    , "description" .= traitDescription
    ]

instance ToJSON Proofs where
  toJSON (Proofs proofs) = object . map fmt . limit $ M.toList proofs
    where
      fmt :: (TraitId, [Assumption]) -> (Text, Value)
      fmt (TraitId _id, assumptions) = (_id, toValue $ collect assumptions)

      collect :: [Assumption] -> ([TheoremId], [TraitId])
      collect [] = ([],[])
      collect (AssumedTheorem t : as) =
        let (theorems, traits) = collect as
        in (t : theorems, traits)
      collect (AssumedTrait t : as) =
        let (theorems, traits) = collect as
        in (theorems, t :  traits)

      toValue :: ([TheoremId], [TraitId]) -> Value
      toValue (is, ts) = toJSON [toJSON is, toJSON ts]

limit :: [a] -> [a]
limit = id -- take 50

instance ToJSON Viewer where
  toJSON Viewer{..} = object
    [ "version"    .= viewerVersion
    , "spaces"     .= limit viewerSpaces
    , "properties" .= limit viewerProperties
    , "theorems"   .= limit viewerTheorems
    , "traits"     .= limit viewerTraits
    , "proofs"     .= viewerProofs
    ]
