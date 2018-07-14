module Core
  ( module Core
  ) where

import Protolude hiding (throwIO)

import Control.Applicative         as Core ((<|>))
import Control.Monad.IO.Class      as Core (MonadIO, liftIO)
import Control.Monad.Logger        as Core (MonadLogger, logDebug, logInfo, logError)
import Control.Monad.Reader        as Core (MonadReader(..), ReaderT, asks, runReaderT)
import Control.Monad.Trans         as Core (lift)
import Control.Monad.Trans.Control as Core (MonadBaseControl)
import Control.Monad.Trans.Except  as Core (ExceptT, except, runExceptT, throwE)
import Data.Aeson                  as Core (FromJSON, ToJSON)
import Data.ByteString             as Core (ByteString)
import Data.Either                 as Core (partitionEithers)
import Data.Either.Combinators     as Core (mapLeft, mapRight)
import Data.Map                    as Core (Map)
import Data.Monoid                 as Core (Monoid)
import Data.Text                   as Core (Text)
import Data.Void                   as Core (Void)
import GHC.Generics                as Core (Generic)
import Git                         as Core (TreeFilePath, MonadGit, Commit, CommitMessage)
import Git.Libgit2                 as Core (LgRepo)
import UnliftIO.Exception          as Core hiding (Handler)

import Class        as Core
import Model        as Core
import Types        as Core
import Types.Store  as Core (Store)

import           Control.Lens hiding ((.=))
import qualified Data.Set     as S

import qualified Formula as F

implicationProperties :: Ord p => Implication p -> S.Set p
implicationProperties (Implication a c) = F.properties a `S.union` F.properties c

theoremIf :: Theorem p -> Formula p
theoremIf t = let (Implication a _) = theoremImplication t in a

theoremThen :: Theorem p -> Formula p
theoremThen t = let (Implication _ c) = theoremImplication t in c

theoremProperties :: Ord p => Theorem p -> S.Set p
theoremProperties = implicationProperties . theoremImplication

theoremName :: Theorem Property -> Text
theoremName t = (F.format propertyName $ theoremIf t) 
             <> " ⇒ "
             <> (F.format propertyName $ theoremThen t)

traitId :: Trait Space Property -> TraitId
traitId = (,) <$> traitSpaceId <*> traitPropertyId

traitSpaceId :: Trait Space p -> SpaceId
traitSpaceId t = spaceId $ t ^. traitSpace

traitPropertyId :: Trait s Property -> PropertyId
traitPropertyId t = propertyId $ t ^. traitProperty

traitName :: Trait Space Property -> Text
traitName Trait{..} = spaceName _traitSpace <> ": " <> label <> propertyName _traitProperty
  where
    label = if _traitValue then "" else "~"

identifyTrait :: Trait Space Property -> Trait SpaceId PropertyId
identifyTrait = over traitSpace spaceId
              . over traitProperty propertyId

(~>) :: F.Formula p -> F.Formula p -> Implication p
(~>) = Implication
infixl 3 ~>

converse :: Implication p -> Implication p
converse (Implication ant con) = Implication con ant

contrapositive :: Implication p -> Implication p
contrapositive (Implication ant con) = Implication (F.negate con) (F.negate ant)

negative :: Implication p -> Implication p
negative (Implication ant con) = Implication (F.negate ant) (F.negate con)

hydrateTheorem :: Ord a => Map a b -> Theorem a -> Either [a] (Theorem b)
hydrateTheorem props theorem =
  let
    (Implication a c) = theoremImplication theorem
  in
    case (F.hydrate props a, F.hydrate props c) of
      (Left as, Left bs) -> Left $ as ++ bs
      (Left as, _) -> Left as
      (_, Left bs) -> Left bs
      (Right a', Right c') -> Right $ theorem { theoremImplication = Implication a' c' }

notFound :: (MonadIO m, Show a) => Text -> a -> m b
notFound resource ident = throwIO $ NotFoundError resource $ show ident