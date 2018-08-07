{-# LANGUAGE ScopedTypeVariables #-}
module Handler.Errors
  ( postErrorR
  , errorHandlers
  , withErrorHandling
  ) where

import Import hiding (notFound)
import Types

import           Data.Aeson
import qualified Data.Text                 as T
import qualified Services.Rollbar          as RB
import qualified UnliftIO.Exception        as U

data ErrorBody = ErrorBody
  { errorCode  :: Text
  , errorToken :: Text
  } deriving (Eq, Show)

instance FromJSON ErrorBody where
  parseJSON = withObject "ErrorBody" $ \e -> ErrorBody
    <$> e .: "code"
    <*> e .: "token"

postErrorR :: Handler Value
postErrorR = withErrorHandling $ do
  ErrorBody{..} <- requireJsonBody
  expectedToken <- getSetting appErrorToken

  unless (errorToken == expectedToken) $
    void $ errorHandler NotFound

  case errorCode of
    "01" -> throwIO ForcedError
    _    -> RB.rollbar RB.Error "Directly sent rollbar message" [ "code" .= errorCode ]

  return $ object []

-- TODO: this should probably be middleware
withErrorHandling :: Handler Value -> Handler Value
withErrorHandling = flip U.catchesDeep errorHandlers

errorHandlers :: [U.Handler Handler Value]
errorHandlers =
  [ h status400 parse
  , h status400 graph
  , h status400 load
  , h status403 permission
  , h status404 notFound
  , h status409 conflict
  , h status422 logic
  , h status422 validation
  ]
  where
    h :: Exception e => Status -> (e -> Text) -> U.Handler Handler Value
    h stat message = U.Handler $ \e -> 
      sendStatusJSON stat $ object [ "error" .= message e ]

conflict :: ConflictError -> Text
conflict ConflictError{..} = "Expected sha " <> expectedSha <> ", but found " <> actualSha

graph :: GraphError -> Text
graph (ExecutionErrors errs) = "Execution errors: " <> tshow errs
graph QueryNameRequired = "Query name is required"
graph (QueryNotFound name) = "Could not find a query named " <> tshow name
graph (QuerySerializationError e) = "Failed to serialize query: " <> T.pack e
graph (SchemaInvalid e) = "Schema invalid: " <> tshow e

load :: LoadError -> Text
load (LoadError path) = "Failed to load " <> decodeUtf8 path

logic :: LogicError -> Text
logic (Contradiction s p expected actual) = 
  "Contradiction: expected (" <> tshow s <> ": " <> tshow p <> ") to have value " 
  <> tshow expected <> ", not " <> tshow actual
logic (Counterexamples sids) = "Theorem has counterexamples: " <> tshow sids
logic (LoadFailure e) = "Failed to load dependencies: " <> tshow e

notFound :: NotFoundError -> Text
notFound NotFoundError{..} = "Failed to find " <> nfResource <> " " <> nfIdentifier

parse :: ParseError -> Text
parse (ParseError path msg) = "Failed to parse " <> decodeUtf8 path <> ": " <> msg

permission :: PermissionError -> Text
permission BranchPermissionRequired{..} =
  "Requires " <> tshow required <> " access on " <> branchName branch <> ", but found " <> tshow actual

validation :: ValidationError -> Text
validation (ValidationMessage msg) = msg