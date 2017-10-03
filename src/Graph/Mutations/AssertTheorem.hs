{-# LANGUAGE DeriveGeneric #-}
module Graph.Mutations.AssertTheorem
  ( AssertTheoremInput
  , assertTheorem
  ) where

import qualified Import
import           Graph.Import
import           Data.Aeson     (decode)
import qualified Data.Text.Lazy as TL

import           Core
import qualified Data         as D
import qualified Data.Theorem as T
import qualified Graph.Types  as G
import qualified Graph.Query  as G

data AssertTheoremInput = AssertTheoremInput
  { antecedent  :: Text
  , consequent  :: Text
  , description :: Text
  } deriving (Show, Generic)

instance FromValue AssertTheoremInput
instance HasAnnotatedInputType AssertTheoremInput
instance Defaultable AssertTheoremInput where
  defaultFor _ = error "No default for AssertTheoremInput"

assertTheorem :: AssertTheoremInput -> G G.Viewer
assertTheorem AssertTheoremInput{..} = do
  (Entity _ user) <- requireToken

  a <- parseFormula antecedent
  c <- parseFormula consequent

  theorem <- Theorem
    <$> D.makeId TheoremId "t"
    <*> pure (Implication a c)
    <*> pure Nothing
    <*> pure description

  let commit = CommitMeta user $ "Add " <> tshow theorem

  T.put (userBranch user) commit theorem >>= either halt G.presentView

parseFormula :: Text -> Import.Handler (Formula PropertyId)
parseFormula text = case decode $ encodeUtf8 $ TL.fromStrict text of
  Nothing -> halt [ ParseError "formula" (show text) ]
  Just f  -> return $ PropertyId <$> f
