module Logic
  ( assertTrait
  , assertTheorem
  , check
  , deduceTraits
  , result
  , search
  , updates
  ) where

import Control.Monad.State.Strict (MonadState, State, gets, modify, runState, state)
import qualified Data.Map.Strict as M
import qualified Data.Set        as S

import Core hiding (force, negate)
import Formula     (negate)
import Util        (fetch, unionN)

type Properties = M.Map PropertyId Bool
type Evidence = (TheoremId, S.Set PropertyId)

newtype Logic a = Logic { unLogic :: State Prover a }
  deriving (Functor, Applicative, Monad, MonadState Prover)

fatal :: LogicError -> Logic a
fatal _err = error "fatal" -- TODO: runLogic $ fatal e >> f == Left e

assertTrait :: Trait Space Property -> Logic ()
assertTrait Trait{..} = do
  let space    = spaceId traitSpace
      property = propertyId traitProperty
  (gets $ Logic.lookup space property . proverView) >>= \case
    -- TODO: error when this is disprovable
    Just val -> if val == traitValue
      then return ()
      else fatal AssertionError
    Nothing -> insertTrait space property traitValue Nothing

assertTheorem :: Theorem PropertyId -> Logic ()
assertTheorem t = do
  cxs <- gets $ counterexamples t . proverView
  if null cxs
    -- TODO: error if theorem is already known (/ deducable?)
    then insertTheorem Nothing t
    else fatal AssertionError

-- TODO: should be able to clean this up
check :: Properties -> Formula PropertyId -> (Match, Set PropertyId)
check ts (Atom p e) = case M.lookup p ts of
  Nothing -> (Unknown, S.empty)
  Just value -> if e == value
     then (Yes, S.singleton p)
     else (No,  S.singleton p)
check ts (And  sf) =
  let
    subs    = map (check ts) sf
    no      = filterMatch No subs
    unknown = filterMatch Unknown subs
  in
    case no of
      Just evidence -> (No, evidence)
      Nothing -> case unknown of
        Just _ -> (Unknown, S.empty)
        Nothing -> (Yes, unionN . map snd $ subs)
check ts (Or sf) =
  let
    subs    = map (check ts) sf
    yes     = filterMatch Yes subs
    unknown = filterMatch Unknown subs
  in
    case yes of
      Just evidence -> (Yes, evidence)
      Nothing -> case unknown of
        Just _ -> (Unknown, S.empty)
        Nothing -> (No, unionN . map snd $ subs)

deduceTraits :: Logic ()
deduceTraits = modify $ \p ->
  let view = proverView p
      idPairs (sid, attrs) = [(sid, pid) | pid <- M.keys attrs]
      newQueue = S.fromList . join . map idPairs . M.toList $ viewTraits view
  in p { proverQueue = proverQueue p ++ newQueue }

search :: Formula PropertyId -> Match -> View -> [SpaceId]
search f mode v = filter matches . M.keys $ viewSpaces v
  where
    matches sid = mode == (fst $ check (attributes sid v) f)

updates :: View -> Logic a -> Either LogicError View
updates before update = (flip viewDiff before) <$> runLogic before update

result :: View -> Logic a -> Either LogicError View
result = runLogic

-- Internal helpers

runLogic :: View -> Logic a -> Either LogicError View
runLogic view handler =
  let (_result, prover) = runState
        (unLogic $ handler >> runQueue checkImplications)
        (initializeProver view)
  -- TODO: handle thrown LogicErrors
  in Right $ proverView prover

viewDiff :: View -> View -> View
viewDiff after before = View
  { viewSpaces     = M.difference (viewSpaces after)     (viewSpaces before)
  , viewProperties = M.difference (viewProperties after) (viewProperties before)
  , viewTheorems   = M.difference (viewTheorems after)   (viewTheorems before)
  , viewProofs     = M.difference (viewProofs after)     (viewProofs before)
  , viewTraits     = M.differenceWith diffTraits (viewTraits after) (viewTraits before)
  , viewVersion    = viewVersion after
  }
  where
    diffTraits a b = Just $ M.difference a b

counterexamples :: Theorem PropertyId -> View -> [SpaceId]
counterexamples t =
  let (Implication ant con) = theoremImplication t
  in  search (And [ant, negate con]) Yes

checkImplications :: SpaceId -> PropertyId -> Logic ()
checkImplications sid pid = do
  thrms <- gets $ relevantTheorems pid
  mapM_ (applyTheorem sid) thrms

applyTheorem :: SpaceId -> Theorem PropertyId -> Logic ()
applyTheorem sid t = do
  props <- gets $ attributes sid . proverView
  case check props ant of
    (No, _)         -> return ()
    (Yes, evidence) -> force con evidence
    (Unknown, _)    -> case check props $ negate con of
      (Yes, evidence) -> force (negate ant) evidence
      _ -> return ()
  where
    tid = theoremId t
    (Implication ant con) = theoremImplication t

    force :: Formula PropertyId -> Set PropertyId -> Logic ()
    force (Atom pid v) evidence = do
      present <- gets $ contains sid pid . proverView
      unless present $
        insertTrait sid pid v (Just (tid, evidence))

    force (And sf) evidence = mapM_ (flip force evidence) sf
    force (Or  sf) evidence = do
      props <- gets $ attributes sid . proverView
      let subs     = map (\f -> (f, check props f)) sf
          yeses    = [f | (f, (    Yes, _)) <- subs]
          unknowns = [f | (f, (Unknown, _)) <- subs]
          extra    = unionN [ev | (_, (No, ev)) <- subs]
      case (yeses, unknowns) of
        ([], [unknown]) -> force unknown (evidence `S.union` extra)
        _ -> return ()

lookup :: MonadThrow m => SpaceId -> PropertyId -> View -> m Bool
lookup s p = fetch p . attributes s

contains :: SpaceId -> PropertyId -> View -> Bool
contains sid pid = M.member pid . attributes sid

attributes :: SpaceId -> View -> Properties
attributes sid View{..} = case M.lookup sid viewTraits of
  Nothing -> M.empty
  Just props -> M.map traitValue props

relevantTheorems :: PropertyId -> Prover -> [Theorem PropertyId]
relevantTheorems pid p = catMaybes $ map (flip M.lookup theorems) theoremIds
  where
    theoremIds = M.findWithDefault [] pid $ proverRelatedTheorems p
    theorems   = viewTheorems $ proverView p

nextId :: Logic Text
nextId = do
  modify $ \p -> p { proverId = proverId p + 1}
  _id <- gets proverId
  return $ "x" <> tshow _id

-- TODO: refactor using lenses
insertTrait :: SpaceId
            -> PropertyId
            -> Bool
            -> Maybe Evidence
            -> Logic ()
insertTrait sid pid value mev = modify $ \prover ->
    let
      view@View{..} = proverView prover
      trait = Trait sid pid value "" -- FIXME: description
    in
      prover
        { proverView = view
          { viewTraits = addNested sid pid trait viewTraits
          , viewProofs = case mev of
                           Just ev -> M.insert (sid, pid) (buildProof trait ev) viewProofs
                           Nothing -> viewProofs
          }
        , proverQueue = S.insert (sid, pid) $ proverQueue prover
        }

addNested :: (Ord k1, Ord k2)
          => k1 -> k2 -> val -> M.Map k1 (M.Map k2 val) -> M.Map k1 (M.Map k2 val)
addNested k1 k2 val m = M.insertWith M.union k1 (M.singleton k2 val) m

insertTheorem :: Maybe TheoremId -> Theorem PropertyId -> Logic ()
insertTheorem mtid i = do
  tid <- maybe (TheoremId <$> nextId) return mtid
  modify $ \p@Prover{..} -> p
    { proverView = proverView
      { viewTheorems = M.insert tid i $ viewTheorems proverView
      }
      -- FIXME: this does not reference tid ...
    , proverRelatedTheorems = addRelatedTheorem proverRelatedTheorems i
    }
  queueTheorem i

filterMatch :: Match -> [(Match, a)] -> Maybe a
filterMatch t pairs = listToMaybe [ts | (m, ts) <- pairs, m == t]

queueTheorem :: Theorem  PropertyId -> Logic ()
queueTheorem t = do
  -- TODO: can tighten up which items are queued to check
  modify $ \p -> p { proverQueue = toCheck (proverView p) `S.union` proverQueue p }
  where
    i@(Implication ant con) = theoremImplication t

    unknowns :: View -> [SpaceId]
    unknowns v = search (Or [ant, con]) Unknown v

    toCheck :: View -> S.Set (SpaceId, PropertyId)
    toCheck u = S.fromList [(s,p) | s <- unknowns u, p <- (S.toList $ implicationProperties i)]


queueNext :: Logic (Maybe (SpaceId, PropertyId))
queueNext = state $ \prover@Prover{..} ->
  case S.maxView proverQueue of
    Just ((s,p), rest) -> (Just (s,p), prover { proverQueue = rest })
    Nothing            -> (Nothing, prover)

runQueue :: (SpaceId -> PropertyId -> Logic ()) -> Logic ()
runQueue handler = queueNext >>= \case
  Just (s,p) -> do
    handler s p
    runQueue handler
  _ -> return ()

initializeProver :: View -> Prover
initializeProver v = Prover
  { proverView            = v
  , proverQueue           = mempty
  , proverRelatedTheorems = foldl' addRelatedTheorem mempty . M.elems $ viewTheorems v
  , proverId              = 1
  }

addRelatedTheorem :: M.Map PropertyId [TheoremId] -> Theorem PropertyId -> Map PropertyId [TheoremId]
addRelatedTheorem m t = S.foldr' (\p -> M.insertWith (++) p [theoremId t]) m $ theoremProperties t

buildProof :: Trait SpaceId PropertyId -> Evidence -> Assumptions
buildProof trait (thrmId, props) = Assumptions
    { assumedTraits   = S.map (\pid -> (traitSpace trait, pid)) props
    , assumedTheorems = S.singleton thrmId
    }
