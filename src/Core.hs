module Core
  ( module Core
  ) where

import Import as Core

import Class                    as Core ()
import Control.Monad.Catch      as Core (MonadCatch, MonadMask)
import Data.Access              as Core (Access(..))
import Data.Citation            as Core (Citation(..), CitationType(..))
import Data.Commit              as Core (Commit(..))
import Data.Formula             as Core (Formula(..))
import Data.Id                  as Core (Id(..))
import Data.Implication         as Core (Implication(..), antecedent, consequent)
import Data.Match               as Core (Match(..))
import Data.Property            as Core (Property'(Property), Property, PropertyId)
import Data.PullRequest         as Core (PullRequest(PullRequest), PullRequestError(PullRequestError))
import Data.Space               as Core (Space'(Space), Space, SpaceId)
import Data.Theorem             as Core (Theorem'(Theorem), Theorem, TheoremId)
import Data.Trait               as Core (Trait'(Trait), Trait, TraitId)
import Database.Persist         as Core (Entity(..))
import Database.Persist.Sql     as Core (fromSqlKey, toSqlKey)
import Persist.Backend.DB.Model as Core
import Types                    as Core

import qualified Data.Map  as Map
import qualified Data.Text as Text
import qualified Prelude

data ParseError = ParseError String
  deriving (Show, Eq, Typeable)

instance Exception ParseError

error :: Text -> a
error = Prelude.error . Text.unpack

indexBy :: Ord k => (a -> k) -> [a] -> Map k a
indexBy f as = Map.fromList $ map (\a -> (f a, a)) as