module Graph.Import
  ( module Graph.Import
  ) where

import GraphQL          as Graph.Import
import GraphQL.API      as Graph.Import
import GraphQL.Resolver as Graph.Import

import Import as Graph.Import hiding (Handler, Enum, Field, Response, Value)
import qualified Import (Handler)

type G a = Handler Import.Handler a

halt :: MonadHandler m => String -> m a
halt msg = sendStatusJSON badRequest400 $ object
  [ "errors" .=
    [ object [
      "message" .= msg
    ] ]
  ]
