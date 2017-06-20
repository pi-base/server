module Logic
  ( Logic
  , assertTrait
  , assertTheorem
  , check
  , checkAllTraits
  , result
  , search
  , updates
  ) where

import Control.Lens hiding (contains)
import Control.Monad.State.Strict (MonadState, State, get, runState, state)
import Control.Monad.Trans.Except (throwE)
import qualified Data.Map.Strict as M
import qualified Data.Set        as S

import Core hiding (force, negate)
import Formula     (negate)
import Util        (fetch, unionN)

type Properties = M.Map PropertyId Bool
type Evidence   = (TheoremId, Set PropertyId)

newtype Logic a = Logic { unLogic :: ExceptT LogicError (State Prover) a }
  deriving (Functor, Applicative, Monad, MonadState Prover)

instance MonadReader Prover Logic where
  ask = get
  local f la = state (\prover -> ((), f prover)) >> la

data Prover = Prover
  { _proverView            :: View
  , _proverRelatedTheorems :: M.Map PropertyId [TheoremId]
  , _proverQueue           :: Set TraitId
  , _proverUpdatedTraits   :: Set TraitId
  , _proverUpdatedTheorems :: Set TheoremId
  }

makeLenses ''Prover

fatal :: LogicError -> Logic a
fatal = Logic . throwE

assertTrait :: Trait SpaceId PropertyId -> Logic ()
assertTrait Trait{..} = do
  (uses proverView $ Logic.lookup _traitSpace _traitProperty) >>= \case
    Just val -> unless (val == _traitValue) $ fatal AssertionError
    Nothing  -> insertTrait _traitSpace _traitProperty _traitValue Nothing

assertTheorem :: Theorem PropertyId -> Logic ()
assertTheorem t = do
  cxs <- uses proverView $ counterexamples t
  if null cxs
    -- TODO: error if theorem is already known (/ deducable?)
    then insertTheorem t
    else fatal AssertionError

-- TODO: should be able to clean this up
check :: Properties -> Formula PropertyId -> (Match, Set PropertyId)
check ts (Atom p e) = case M.lookup p ts of
  Just value -> if e == value
    then (Yes, S.singleton p)
    else (No,  S.singleton p)
  Nothing -> (Unknown, S.empty)
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

checkAllTraits :: Logic ()
checkAllTraits = do
  traits <- use $ proverView . viewTraits
  proverQueue <>= S.fromList [(sid, pid) | (sid, attrs) <- M.toList traits, pid <- M.keys attrs]

search :: Formula PropertyId -> Match -> View -> [SpaceId]
search f mode v = filter matches . M.keys $ v ^. viewSpaces
  where
    matches sid = mode == (fst $ check (attributes sid v) f)

updates :: View -> Logic a -> Either LogicError View
updates v l = updatedView <$> runLogic v l

result :: View -> Logic a -> Either LogicError View
result v l = _proverView <$> runLogic v l

-- Internal helpers

runLogic :: View -> Logic a -> Either LogicError Prover
runLogic v handler =
  case runState stateHandler initialState of
    (Left err, _)      -> Left err
    (Right (), prover) -> Right prover
  where
    stateHandler = runExceptT . unLogic $ handler >> runQueue
    initialState = initializeProver v

counterexamples :: Theorem PropertyId -> View -> [SpaceId]
counterexamples t =
  let (Implication ant con) = theoremImplication t
  in  search (And [ant, negate con]) Yes

applyTheorem :: SpaceId -> Theorem PropertyId -> Logic ()
applyTheorem sid t = do
  props <- uses proverView $ attributes sid
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
      present <- uses proverView $ contains sid pid
      unless present $
        insertTrait sid pid v (Just (tid, evidence))

    force (And sf) evidence = mapM_ (flip force evidence) sf
    force (Or  sf) evidence = do
      props <- uses proverView $ attributes sid
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
attributes sid View{..} = case M.lookup sid _viewTraits of
  Nothing    -> M.empty
  Just props -> M.map _traitValue props

insertTrait :: SpaceId
            -> PropertyId
            -> Bool
            -> Maybe Evidence
            -> Logic ()
insertTrait sid pid value mev = do
  proverView . viewTraits %= addNested sid pid trait
  proverView . viewProofs %= addProof mev
  proverUpdatedTraits <>= S.singleton (sid, pid)
  proverQueue         <>= S.singleton (sid, pid)
  where
    trait = Trait sid pid value "" -- FIXME: description

    addProof (Just ev) = M.insert (sid, pid) (buildProof trait ev)
    addProof Nothing   = id

-- TODO: remove this once everything is lens-y
addNested :: (Ord k1, Ord k2)
          => k1 -> k2 -> val -> M.Map k1 (M.Map k2 val) -> M.Map k1 (M.Map k2 val)
addNested k1 k2 val m = M.insertWith M.union k1 (M.singleton k2 val) m

insertTheorem :: Theorem PropertyId -> Logic ()
insertTheorem t = do
  v <- use proverView
  proverView . viewTheorems %= M.insert (theoremId t) t
  proverRelatedTheorems     %= flip addRelatedTheorem t
  proverUpdatedTheorems    <>= S.singleton (theoremId t)
  proverQueue              <>= toCheck v
  where
    -- TODO: can tighten up which items are queued to check
    unknowns :: View -> [SpaceId]
    unknowns v = search (And [theoremIf t, theoremThen t]) Unknown v

    toCheck :: View -> Set (SpaceId, PropertyId)
    toCheck v = S.fromList [(s,p) | s <- unknowns v, p <- (S.toList $ theoremProperties t)]

filterMatch :: Match -> [(Match, a)] -> Maybe a
filterMatch t pairs = listToMaybe [ts | (m, ts) <- pairs, m == t]

queueNext :: Logic (Maybe (SpaceId, PropertyId))
queueNext = proverQueue %%= \q -> case S.maxView q of
  Just ((s,p), rest) -> (Just (s,p), rest)
  Nothing            -> (Nothing, S.empty)

runQueue :: Logic ()
runQueue = queueNext >>= \case
  Nothing -> return ()
  Just (s,p) -> do
    -- TODO: batch up queue by property to avoid recomputing related theorem data
    relatedTheoremIds <- uses proverRelatedTheorems $ M.findWithDefault [] p
    allTheorems       <- use $ proverView . viewTheorems
    let relatedTheorems = catMaybes $ map (flip M.lookup allTheorems) relatedTheoremIds
    mapM_ (applyTheorem s) relatedTheorems
    runQueue

initializeProver :: View -> Prover
initializeProver v = Prover
  { _proverView            = v
  , _proverQueue           = mempty
  , _proverUpdatedTraits   = mempty
  , _proverUpdatedTheorems = mempty
  , _proverRelatedTheorems = foldl' addRelatedTheorem mempty . M.elems $ _viewTheorems v
  }

addRelatedTheorem :: M.Map PropertyId [TheoremId] -> Theorem PropertyId -> Map PropertyId [TheoremId]
addRelatedTheorem m t = S.foldr' (\p -> M.insertWith (++) p [theoremId t]) m $ theoremProperties t

buildProof :: Trait SpaceId PropertyId -> Evidence -> Proof
buildProof trait (thrmId, props) = Proof (_traitSpace trait) props (S.singleton thrmId)

updatedView :: Prover -> View
updatedView prover = filterView
  (prover ^. proverView)
  (prover ^. proverUpdatedTraits)
  (prover ^. proverUpdatedTheorems)

filterView :: View -> Set TraitId -> Set TheoremId -> View
filterView View{..} traitIds theoremIds =
  let
    acc (s, p) (ss, ps) = (S.insert s ss, S.insert p ps)
    (spaceIds, traitPropertyIds) = S.foldr' acc (S.empty, S.empty) traitIds
    theorems = slice theoremIds _viewTheorems
    theoremPropertyIds = unionN . map theoremProperties $ M.elems theorems
    propertyIds = traitPropertyIds <> theoremPropertyIds
  in View
       { _viewProperties = slice propertyIds _viewProperties
       , _viewSpaces     = slice spaceIds    _viewSpaces
       , _viewTheorems   = slice theoremIds _viewTheorems
       , _viewProofs     = slice traitIds   _viewProofs
       -- FIXME: this is too broad. Should map over (s,p) pairs only
       , _viewTraits     = M.map (slice traitPropertyIds) $ slice spaceIds _viewTraits
       , _viewVersion    = Nothing
       }

slice :: Ord k => Set k -> Map k v -> Map k v
slice ks = M.filterWithKey (\k _ -> S.member k ks)
