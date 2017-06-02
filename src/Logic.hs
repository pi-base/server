module Logic
  ( assertTrait
  , assertTheorem
  , U.check
  , deduceTraits
  , U.search
  ) where

import Control.Monad.State.Strict (State, gets)
import qualified Data.Set as S

import Core hiding (force, negate)
import Formula
import qualified Logic.Universe as U
import Util (unionN)
import Viewer

deduceTraits :: Viewer -> [(Trait Space Property, Assumptions)]
deduceTraits v = U.withQueue checkImplications $ U.fromViewer v

counterexamples :: Implication PropertyId -> U.Universe -> [SpaceId]
counterexamples (Implication ant con) = U.search f Yes
  where
    f = And [ant, negate con]

assertTrait :: Trait Space Property -> State U.Universe ()
assertTrait Trait{..} = do
  let space    = spaceId traitSpace
      property = propertyId traitProperty
  (gets $ U.lookup space property) >>= \case
    -- TODO: error when this is disprovable
    Just  _ -> return ()
    Nothing -> U.insertTrait Nothing space property traitValue Nothing

assertTheorem :: Implication PropertyId -> State U.Universe ()
assertTheorem i = do
  cxs <- gets $ counterexamples i
  -- TODO: error when this is disprovable
  when (null cxs) $ U.insertTheorem Nothing i

checkImplications :: SpaceId -> PropertyId -> State U.Universe ()
checkImplications sid pid = (gets $ U.relevantTheorems pid) >>= mapM_ (applyTheorem sid)

applyTheorem :: SpaceId -> (TheoremId, Implication PropertyId) -> State U.Universe ()
applyTheorem sid (tid, (Implication ant con)) = do
  props <- gets $ U.attributes sid
  case U.check props ant of
    (No, _)         -> return ()
    (Yes, evidence) -> force con evidence
    (Unknown, _)    -> case U.check props $ negate con of
      (Yes, evidence) -> force (negate ant) evidence
      _ -> return ()

  where
    force :: Formula PropertyId -> Set PropertyId -> State U.Universe ()
    force (Atom pid v) evidence = do
      present <- gets $ U.contains sid pid
      unless present $
        U.insertTrait Nothing sid pid v (Just (tid, evidence))

    force (And sf) evidence = mapM_ (flip force evidence) sf
    force (Or  sf) evidence = do
      props <- gets $ U.attributes sid
      let subs     = map (\f -> (f, U.check props f)) sf
          yeses    = [f | (f, (    Yes, _)) <- subs]
          unknowns = [f | (f, (Unknown, _)) <- subs]
          extra    = unionN [ev | (_, (No, ev)) <- subs]
      case (yeses, unknowns) of
        ([], [unknown]) -> force unknown (evidence `S.union` extra)
        _ -> return ()
