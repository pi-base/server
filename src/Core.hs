module Core
  ( module Core
  ) where

import Import as Core

import Control.Monad.Logger as Core (LogLevel(..))
import Database.Persist     as Core (Entity(..))

import Class            as Core
import Data.Store.Types as Core (Store)
import Model            as Core
import Settings         as Core
import Types            as Core

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

notFound :: MonadIO m => Text -> Text -> m b
notFound resource ident = throwIO $ NotFoundError resource ident