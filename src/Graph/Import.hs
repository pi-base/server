module Graph.Import
  ( module Graph.Import
  ) where

import GraphQL                 as Graph.Import
import GraphQL.API             as Graph.Import
import GraphQL.Resolver        as Graph.Import
import GraphQL.Value.FromValue as Graph.Import (FromValue(..))

import Import as Graph.Import hiding (Handler, Enum, Field, Response, Value)

import Core            as Graph.Import (MonadDB(..), MonadGraph(..), Error(GraphError), GraphError(..))
import Data            as Graph.Import (slugify)
import Handler.Helpers as Graph.Import

import qualified Core        (Error, explainError)
import           Graph.Class ()

-- TODO: status should change depending on error type
halt :: MonadHandler m => [Core.Error] -> m a
halt errs = sendStatusJSON badRequest400 $ object [ "errors" .= map render errs ]
  where
    render err = object [ "message" .= Core.explainError err ]