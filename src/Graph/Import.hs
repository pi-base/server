module Graph.Import
  ( module Graph.Import
  ) where

import GraphQL                 as Graph.Import
import GraphQL.API             as Graph.Import
import GraphQL.Resolver        as Graph.Import
import GraphQL.Value.FromValue as Graph.Import (FromValue(..))

import Import as Graph.Import hiding (Handler, Enum, Field, Response, Value)

import Handler.Helpers as Graph.Import

import qualified Import (Handler)

type G a = Handler Import.Handler a

halt :: MonadHandler m => String -> m a
halt msg = sendStatusJSON badRequest400 $ object
  [ "errors" .=
    [ object [
      "message" .= msg
    ] ]
  ]
