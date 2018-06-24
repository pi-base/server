module Data.Parse
  ( property
  , propertyIds
  , space
  , spaceIds
  , spaceTraitIds
  , theorem
  , theoremIds
  , trait
  , traitIds
  --
  , sourceCommitEntries
  , blobs
  ) where

import Protolude hiding (throwIO)
import Conduit
import qualified Data.ByteString.Char8 as BS8
import Data.Attoparsec.Text hiding (space)
import Git

import           Core
import           Data.Git (getDir)
import qualified Page
import qualified Page.Property
import qualified Page.Space
import qualified Page.Theorem
import qualified Page.Trait

space :: MonadStore m => Commit LgRepo -> SpaceId -> m Space
space commit sid = load commit path Page.Space.page
  where path = "spaces/" <> unId sid <> "/README.md"

property :: MonadStore m => Commit LgRepo -> PropertyId -> m Property
property commit pid = load commit path Page.Property.page
  where path = "properties/" <> unId pid <> ".md"

theorem :: MonadStore m => Commit LgRepo -> TheoremId -> m (Theorem PropertyId)
theorem commit tid = load commit path Page.Theorem.page
  where path = "theorems/" <> unId tid <> ".md"

trait :: MonadStore m
      => Commit LgRepo
      -> SpaceId
      -> PropertyId
      -> m (Trait SpaceId PropertyId)
trait commit sid pid = load commit path Page.Trait.page
  where path = "spaces/" <> unId sid <> "/properties/" <> unId pid <> ".md"

propertyIds :: MonadStore m => Commit LgRepo -> ConduitM () PropertyId m ()
propertyIds commit = sourceCommitEntries commit ["properties"]
                  .| parsePath propertyIdParser

spaceIds :: MonadStore m => Commit LgRepo -> ConduitM () SpaceId m ()
spaceIds commit = sourceCommitEntries commit ["spaces"]
               .| parsePath spaceIdParser

theoremIds :: MonadStore m => Commit LgRepo -> ConduitM () TheoremId m ()
theoremIds commit = sourceCommitEntries commit ["theorems"]
                 .| parsePath theoremIdParser

traitIds :: MonadStore m => Commit LgRepo -> ConduitM () (SpaceId, PropertyId) m ()
traitIds commit = sourceCommitEntries commit ["spaces"]
               .| parsePath traitIdParser

spaceTraitIds :: MonadStore m => SpaceId -> Commit LgRepo -> ConduitM () PropertyId m ()
spaceTraitIds _id commit = sourceCommitEntries commit ["spaces", encodeUtf8 (unId _id), "properties"]
                        .| parsePath traitIdParser
                        .| mapC snd

load :: MonadStore m => Commit LgRepo -> Text -> Page a -> m a
load commit path page = do
  tree <- lookupTree $ commitTree commit
  treeEntry tree (encodeUtf8 path) >>= \case
    Just (BlobEntry oid _) -> do
      blob <- catBlobUtf8 oid
      either throwIO return $ Page.parse page (encodeUtf8 path, blob)
    _ -> notFound "Tree" path

parsePath :: Monad m => Parser a -> ConduitM (TreeFilePath, b) a m ()
parsePath parser = awaitForever $ \(path, _) -> case parseOnly parser (decodeUtf8 path) of
  Left _ -> return ()
  Right parsed -> yield parsed

spaceIdParser :: Parser SpaceId
spaceIdParser = do
  _id <- "spaces/" *> takeTill (== '/') <* "/README.md"
  return $ Id _id

propertyIdParser :: Parser PropertyId
propertyIdParser = do
  _id <- "properties/" *> takeTill (== '.') <* ".md"
  return $ Id _id

theoremIdParser :: Parser TheoremId
theoremIdParser = do
  _id <- "theorems/" *> takeTill (== '.') <* ".md"
  return $ Id _id

traitIdParser :: Parser (SpaceId, PropertyId)
traitIdParser = do
  _   <- "spaces/"
  sid <- takeTill (== '/')
  _   <- "/properties/"
  pid <- takeTill (== '.')
  _   <- ".md"
  return (Id sid, Id pid)

sourceCommitEntries :: MonadGit r m
                    => Commit r
                    -> [TreeFilePath]
                    -> ConduitM i (TreeFilePath, TreeEntry r) m ()
sourceCommitEntries commit path = do
  mdir <- lift $ do
    tree <- lookupTree $ commitTree commit
    getDir tree path
  case mdir of
    Nothing  -> return ()
    Just dir -> sourceTreeEntries dir 
      .| mapC (\(p,t) -> (format p, t))
  where 
    format :: TreeFilePath -> TreeFilePath
    format part = BS8.intercalate "/" $ path ++ [part]

blobs :: MonadGit r m => ConduitM (TreeFilePath, TreeEntry r) (TreeFilePath, Text) m ()
blobs = awaitForever $ \(path, entry) -> case entry of
  (BlobEntry _id _) -> do
    blob <- lift $ catBlobUtf8 _id
    yield (path, blob)
  _ -> return ()
