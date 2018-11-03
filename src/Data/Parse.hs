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
  ) where

import           Core hiding (find)

import           Conduit
import qualified Data.ByteString.Char8 as BS8
import           Data.Attoparsec.Text hiding (space)
import           Git

import           Data.Git (getDir)
import qualified Page
import qualified Page.Property
import qualified Page.Space
import qualified Page.Theorem
import qualified Page.Trait

space :: Git m => Tree LgRepo -> SpaceId -> m Space
space tree sid = load tree path Page.Space.page
  where path = "spaces/" <> unId sid <> "/README.md"

property :: Git m => Tree LgRepo -> PropertyId -> m Property
property tree pid = load tree path Page.Property.page
  where path = "properties/" <> unId pid <> ".md"

theorem :: Git m => Tree LgRepo -> TheoremId -> m (Theorem PropertyId)
theorem tree tid = load tree path Page.Theorem.page
  where path = "theorems/" <> unId tid <> ".md"

trait :: Git m
      => Tree LgRepo
      -> SpaceId
      -> PropertyId
      -> m (Trait SpaceId PropertyId)
trait tree sid pid = load tree path Page.Trait.page
  where path = "spaces/" <> unId sid <> "/properties/" <> unId pid <> ".md"

propertyIds :: Git m => Tree LgRepo -> ConduitM () PropertyId m ()
propertyIds tree = find tree ["properties"]
                .| parsePath propertyIdParser

spaceIds :: Git m => Tree LgRepo -> ConduitM () SpaceId m ()
spaceIds tree = find tree ["spaces"]
             .| parsePath spaceIdParser

theoremIds :: Git m => Tree LgRepo -> ConduitM () TheoremId m ()
theoremIds tree = find tree ["theorems"]
               .| parsePath theoremIdParser

traitIds :: Git m => Tree LgRepo -> ConduitM () (SpaceId, PropertyId) m ()
traitIds tree = find tree ["spaces"]
             .| parsePath traitIdParser

spaceTraitIds :: Git m => SpaceId -> Tree LgRepo -> ConduitM () PropertyId m ()
spaceTraitIds _id tree = find tree ["spaces", encodeUtf8 (unId _id), "properties"]
                      .| parsePath traitIdParser
                      .| mapC snd

load :: Git m => Tree LgRepo -> Text -> Page a -> m a
load tree path page = treeEntry tree (encodeUtf8 path) >>= \case
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

find :: Git m
     => Tree LgRepo
     -> [TreeFilePath]
     -> ConduitM i (TreeFilePath, TreeEntry LgRepo) m ()
find tree path = lift (getDir tree path) >>= \case
  Nothing  -> return ()
  Just dir -> sourceTreeEntries dir .| mapC (\(p,t) -> (format p, t))
  where
    format :: TreeFilePath -> TreeFilePath
    format part = BS8.intercalate "/" $ path ++ [part]
