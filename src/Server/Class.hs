{-# LANGUAGE DeriveAnyClass #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Server.Class () where

import Server.Import hiding (ServerT)

import qualified Data.Branch      as Branch
import qualified Data.Property    as Property
import           Data.PullRequest (PullRequest(..), PullRequestError(..))
import qualified Data.Space       as Space
import qualified Data.Theorem     as Theorem
import qualified Data.Trait       as Trait
import           Data.Aeson       (FromJSON(..), ToJSON(..), (.=), (.:), object, withObject)
import qualified Data.Id          as Id
import           Servant          (FromHttpApiData(..))

instance FromJSON (Implication PropertyId) where
  parseJSON = withObject "Implication" $ \o ->
    Implication
      <$> o .: "if"
      <*> o .: "then"

instance ToJSON (Implication PropertyId) where
  toJSON (Implication ant con) = object
    [ "if"   .= ant
    , "then" .= con
    ]

deriving instance ToJSON   CreateSpaceBody
deriving instance FromJSON CreateSpaceBody
deriving instance ToJSON   UpdateSpaceBody
deriving instance FromJSON UpdateSpaceBody
deriving instance ToJSON   CreatePropertyBody
deriving instance FromJSON CreatePropertyBody
deriving instance ToJSON   UpdatePropertyBody
deriving instance FromJSON UpdatePropertyBody
deriving instance ToJSON   CreateTheoremBody
deriving instance FromJSON CreateTheoremBody
deriving instance ToJSON   UpdateTheoremBody
deriving instance FromJSON UpdateTheoremBody
deriving instance ToJSON   CreateTraitBody
deriving instance FromJSON CreateTraitBody
deriving instance ToJSON   UpdateTraitBody
deriving instance FromJSON UpdateTraitBody

instance Id.Encodable a => ToHttpApiData (Id a) where
  toUrlPiece = Id.encode

instance Id.Encodable a => FromHttpApiData (Id a) where
  parseUrlPiece = Id.decode

instance ToJSON Status where
  toJSON Status{..} = object
    [ "ok"      .= True
    , "started" .= started
    , "build"   .= buildInfo
    ]

instance FromJSON Status where
  parseJSON = withObject "Status" $ \s -> do
    started   <- s .: "started"
    buildInfo <-  s .: "build"
    return Status{..}

instance ToJSON Branch where
  toJSON Branch{..} = object
    [ "name" .= _name
    ]

instance FromJSON Branch where
  parseJSON = withObject "Branch" $ \b -> do
    _name <- b .: "name"
    return Branch{..}

instance ToJSON Space where
  toJSON Space{..} = object
    [ "uid"         .= id
    , "name"        .= name
    , "aliases"     .= aliases
    , "description" .= description
    , "topology"    .= topology
    , "refs"        .= refs
    ]

instance FromJSON Space where
  parseJSON = withObject "Space" $ \o -> do
    id          <- o .: "uid"
    name        <- o .: "name"
    aliases     <- o .: "aliases"
    description <- o .: "description"
    topology    <- o .: "topology"
    refs        <- o .: "refs"
    return Space{..}

instance ToJSON Property where
  toJSON Property{..} = object
    [ "uid"         .= id
    , "name"        .= name
    , "aliases"     .= aliases
    , "description" .= description
    , "refs"        .= refs
    ]

instance FromJSON Property where
  parseJSON = withObject "Property" $ \o -> do
    id          <- o .: "uid"
    name        <- o .: "name"
    aliases     <- o .: "aliases"
    description <- o .: "description"
    refs        <- o .: "refs"
    return Property{..}

instance ToJSON Theorem where
  toJSON Theorem{..} = object
    [ "uid"         .= id
    , "if"          .= antecedent implication
    , "then"        .= consequent implication
    , "converse"    .= converse
    , "description" .= description
    , "refs"        .= refs
    ]

instance FromJSON Theorem where
  parseJSON = withObject "Theorem" $ \o -> do
    implication <- Implication
                     <$> o .: "if"
                     <*> o .: "then"
    id          <- o .: "uid"
    converse    <- o .: "converse"
    description <- o .: "description"
    refs        <- o .: "refs"
    return Theorem{..}

instance ToJSON Trait where
  toJSON Trait{..} = object
    [ "space"       .= space
    , "property"    .= property
    , "value"       .= value
    , "description" .= description
    , "refs"        .= refs
    ]

instance FromJSON Trait where
  parseJSON = withObject "Trait" $ \o -> do
    space       <- o .: "space"
    property    <- o .: "property"
    value       <- o .: "value"
    description <- o .: "description"
    refs        <- o .: "refs"
    return Trait{..}

instance ToJSON View where
  toJSON View{..} = object
    [ "spaces"     .= spaces
    , "properties" .= properties
    , "theorems"   .= theorems
    , "traits"     .= traits
    , "version"    .= version
    ]

instance FromJSON View where
  parseJSON = withObject "View" $ \v -> do
    spaces     <- v .: "spaces"
    properties <- v .: "properties"
    theorems   <- v .: "theorems"
    traits     <- v .: "traits"
    version    <- v .: "version"
    return View{..}

instance ToJSON PullRequest where
  toJSON PullRequest{..} = object
    [ "url" .= url
    ]

instance FromJSON PullRequest where
  parseJSON = withObject "PullRequest" $ \o -> do
    url <- o .: "url"
    return PullRequest{..}

instance ToJSON PullRequestError where
  toJSON PullRequestError{..} = object
    [ "message" .= message
    ]

instance FromJSON PullRequestError where
  parseJSON = withObject "PullRequestError" $ \o -> do
    message <- o .: "message"
    return PullRequestError{..}
