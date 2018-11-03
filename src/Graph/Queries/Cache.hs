{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
module Graph.Queries.Cache
  ( QueryCache
  , fetch
  , mkCache
  , schema
  ) where

import Protolude hiding (check, throwIO)

import qualified Data.Map              as M
import qualified Data.Text             as T
import           Data.Text.IO          (readFile)
import           GraphQL
import           GraphQL.Internal.Name (Name(..))
import           GraphQL.Introspection (serialize)
import           System.FilePath       (takeBaseName)

import           Graph.Class         ()
import qualified Graph.Schema        as G
import           Graph.Serialization (writeSchema)
import           Graph.Types         (QueryCache(..), Query)
import           Util                (traverseDir)

{-
  This is a bit of an abuse, but we want to be sure that the schema file
  on disk always represents the compiled schema
-}
schema :: Text
schema = $(writeSchema $ serialize @(G.Root Identity)) -- I don't love that we need a concrete monad here ...

{-
  Assumes a file structure of
    - <root>
      - queries
        - Viewer.gql
        - Me.gql
      - mutations
        - AssertSpace.gql
        - ...
  with each file having the same name as the query or mutation it defines

  TODO: check operation names of parsed documents
  TODO: make this a TemplateHaskell loader that watches files
-}
mkCache :: MonadIO m => FilePath -> m (Either [QueryError] QueryCache)
mkCache root = runExceptT $ do
  querySchema    <- check $ makeSchema @G.QueryRoot
  qs             <- compile querySchema (root <> "/queries")
  mutationSchema <- check $ makeSchema @G.MutationRoot
  ms             <- compile mutationSchema (root <> "/mutations")
  queries        <- validate $ qs <> ms
  return QueryCache{..}
  where
    check :: Monad m => Either e a -> ExceptT [e] m a
    check (Left  e) = ExceptT . return $ Left [e]
    check (Right a) = return a

    compile :: MonadIO m => Schema -> FilePath -> m (Map Text (Either QueryError Query))
    compile s dir = liftIO $ traverseDir (add s) dir mempty

    add :: Schema
        -> Map Text (Either QueryError Query)
        -> FilePath
        -> IO (Map Text (Either QueryError Query))
    add s acc path = do
      result <- load s path
      return $ M.insert (T.pack $ takeBaseName path) result acc

    validate :: (Monad m, Ord k)
             => Map k (Either e a)
             -> ExceptT [e] m (Map k a)
    validate m = case M.foldrWithKey gather (mempty, mempty) m of
      ([], queries) -> return queries
      (errs, _) -> ExceptT . return $ Left errs

    gather :: Ord k => k -> Either e a -> ([e], Map k a) -> ([e], Map k a)
    gather _ (Left  e) (es, m) = (e:es, m)
    gather k (Right a) (es, m) = (es, M.insert k a m)

fetch :: QueryCache -> Name -> Maybe Query
fetch QueryCache{..} name = M.lookup (unName name) queries

-- Helpers

load :: MonadIO m
     => Schema
     -> FilePath
     -> m (Either QueryError Query)
load s path = compileQuery s <$> liftIO (readFile path)