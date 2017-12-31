module Graph.Queries.Cache
  ( Cache
  , loadAll
  , mkCache
  , mutation
  , Graph.Queries.Cache.query
  ) where

import Graph.Import hiding (Query, readFile)
import Graph.Types  hiding (Error)

import           Data.Attoparsec.Text
import           Data.Either.Combinators     (rightToMaybe)
import qualified Data.Map                    as M
import qualified Data.Text                   as T
import           Data.Text.IO                (readFile)
import           GraphQL.Internal.Syntax.AST (Name(..), makeName)

import Util (traverseDir)

data CacheError = InvalidPathName FilePath | CompilationError QueryError
  deriving (Eq, Show)

data Cache = Cache
  { root :: FilePath
  , schema :: Schema
  , queries :: IORef (Map Name Query)
  }

mkCache :: (MonadBase IO m, MonadIO m) => Schema -> FilePath -> m Cache
mkCache schema root = Cache 
  <$> pure root
  <*> pure schema
  <*> newIORef mempty

loadAll :: (MonadBase IO m, MonadIO m) => Cache -> m (Map FilePath CacheError)
loadAll cache = liftIO $ traverseDir f (root cache) mempty
  where
    f :: Map FilePath CacheError -> FilePath -> IO (Map FilePath CacheError)
    f acc path = case parseName (root cache) path of
      Nothing   -> return $ M.insert path (InvalidPathName path) acc
      Just name -> do
        result <- load cache name path
        case result of
          Left err -> return $ M.insert path (CompilationError err) acc
          Right  _ -> return $ acc

query :: (MonadBase IO m, MonadIO m) => Cache -> Name -> m (Maybe Query)
query cache name = do
  qs <- readIORef (queries cache)
  case M.lookup name qs of
    Just q  -> return $ Just q
    Nothing ->
      let path = root cache ++ "/" ++ (T.unpack $ unName name) ++ ".gql"
      in rightToMaybe <$> load cache name path

mutation :: (MonadBase IO m, MonadIO m) => Cache -> Name -> m (Maybe Query)
mutation = Graph.Queries.Cache.query -- for now at least

-- Helpers

load :: (MonadBase IO m, MonadIO m) 
     => Cache 
     -> Name
     -> FilePath 
     -> m (Either QueryError Query)
load Cache{..} name path = do
    contents <- liftIO . readFile $ path
    case compileQuery schema contents of
      Left    err -> return $ Left err
      Right query -> do
        modifyIORef' queries $ M.insert name query
        return $ Right query

parseName :: FilePath -> String -> Maybe Name
parseName root str = do
  let parser = do
        _ <- string (T.pack root)
        _ <- "/"
        name <- takeTill (== '.')
        _ <- ".gql"
        return name
  parsed <- maybeResult . parse parser $ T.pack str
  rightToMaybe $ makeName parsed
