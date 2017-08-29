module Logic
  ( LogicT
  , Deductions(..)
  , Evidence(..)
  , runLogicT
  , assertTrait
  , assertTheorem
  , checkAll
  ) where

import Control.Lens hiding (contains)
import Control.Monad.State.Strict     (MonadState, State, get, runState, state)
import Control.Monad.Trans.Except     (throwE)
import Control.Monad.Trans.RWS.Strict (RWST, runRWST)
import qualified Data.Map.Strict as M
import qualified Data.Set        as S

import Core hiding (force, negate)
import Formula     (negate)
import Util        (fetch, unionN)

data Evidence = Asserted
              | Deduced (Set TheoremId) (Set PropertyId)
              deriving Show

newtype LogicT m a = LogicT
  { unLogicT :: ExceptT LogicError (RWST (Loader m) () Prover m) a }
  deriving (Functor, Applicative, Monad, MonadState Prover, MonadReader (Loader m))

instance MonadTrans LogicT where
  lift = LogicT . lift . lift

-- TODO: lensify
data Deductions = Deductions
  { deductionTheorems :: [Theorem PropertyId]
  , deductionTraits   :: Map TraitId (TVal, Evidence)
  } deriving Show

data Prover = Prover
  { _proverTheorems        :: Map TheoremId (Implication PropertyId)
  , _proverRelatedTheorems :: Map PropertyId [TheoremId]
  , _proverDeductions      :: Deductions
  }

makeLenses ''Prover

getLoader :: Monad m => LogicT m (Loader m)
getLoader = LogicT $ lift $ ask

loadSpace :: Monad m => SpaceId -> LogicT m Properties
loadSpace id = do
  loader <- getLoader
  (lift $ loaderSpace loader id) >>= \case
    Left  err   -> fatal $ LoadFailure err
    Right props -> return props

load :: Monad m => m (Either LoadError b) -> LogicT m b
load f = lift f >>= either (fatal . LoadFailure) return

loadEachSpace :: Monad m => ((SpaceId, Properties) -> LogicT m a) -> LogicT m ()
loadEachSpace f = do
  loader   <- getLoader
  spaceIds <- load $ loaderSpaceIds loader
  forM_ spaceIds $ \sid -> void $ do
    props <- load $ loaderSpace loader sid
    f (sid, props)

assertTrait :: Monad m => Trait SpaceId PropertyId -> LogicT m ()
assertTrait Trait{..} = do
  props <- loadSpace _traitSpace
  case M.lookup _traitProperty props of
    Just val -> unless (val == _traitValue) $
      fatal $ Contradiction _traitSpace _traitProperty _traitValue val
    Nothing -> do
      props'  <- addTrait _traitSpace _traitProperty _traitValue Asserted props
      related <- relatedTheorems _traitProperty
      void $ foldM (\ps (tid, impl) -> applyTheorem tid impl _traitSpace ps) props' related

assertTheorem :: Monad m => Theorem PropertyId -> LogicT m ()
assertTheorem t = do
  let tid  = theoremId t
      impl = theoremImplication t
  proverTheorems        %= M.insert tid impl
  proverRelatedTheorems %= flip addRelatedTheorem (tid, impl)
  proverDeductions      %= recordTheorem t
  loadEachSpace $ \(space, props) ->
    applyTheorem (theoremId t) (theoremImplication t) space props

checkAll :: Monad m => LogicT m ()
checkAll = do
  theorems <- use $ proverTheorems
  loadEachSpace $ \(space, props) -> do
    foldM (\ps (tid, impl) -> applyTheorem tid impl space ps) props $ M.toList theorems

runLogicT :: Monad m => Loader m -> LogicT m a -> m (Either LogicError Deductions)
runLogicT loader handler = loaderImplications loader >>= \case
  Left err -> return . Left $ LoadFailure err
  Right theorems -> do
    let rwst = runExceptT $ unLogicT handler
    (result, prover, ()) <- runRWST rwst loader $ initializeProver theorems
    case result of
      Left err -> return $ Left err
      Right _  -> return . Right $ _proverDeductions prover

-- Internal helpers

initializeProver :: [(TheoremId, Implication PropertyId)] -> Prover
initializeProver implications = Prover
  { _proverTheorems        = M.fromList implications
  , _proverRelatedTheorems = foldl' addRelatedTheorem mempty implications
  , _proverDeductions      = Deductions mempty mempty
  }

fatal :: Monad m => LogicError -> LogicT m a
fatal = LogicT . throwE

-- TODO: should be able to clean this up
evaluate :: Properties -> Formula PropertyId -> (Match, Set PropertyId)
evaluate ts (Atom p e) = case M.lookup p ts of
  Just value -> if e == value
    then (Yes, S.singleton p)
    else (No,  S.singleton p)
  Nothing -> (Unknown, S.empty)
evaluate ts (And  sf) =
  let
    subs    = map (evaluate ts) sf
    no      = filterMatch No subs
    unknown = filterMatch Unknown subs
  in
    case no of
      Just evidence -> (No, evidence)
      Nothing -> case unknown of
        Just _ -> (Unknown, S.empty)
        Nothing -> (Yes, unionN . map snd $ subs)
evaluate ts (Or sf) =
  let
    subs    = map (evaluate ts) sf
    yes     = filterMatch Yes subs
    unknown = filterMatch Unknown subs
  in
    case yes of
      Just evidence -> (Yes, evidence)
      Nothing -> case unknown of
        Just _ -> (Unknown, S.empty)
        Nothing -> (No, unionN . map snd $ subs)

search :: Formula PropertyId -> Match -> View -> [SpaceId]
search f mode v = filter matches . M.keys $ v ^. viewSpaces
  where
    matches sid = mode == (fst $ evaluate (attributes sid v) f)

applyTheorem :: Monad m => TheoremId -> Implication PropertyId -> SpaceId -> Properties -> LogicT m Properties
applyTheorem tid (Implication ant con) sid props =
  case evaluate props ant of
    (No, _)         -> return props
    (Yes, evidence) -> force con evidence props
    (Unknown, _)    -> case evaluate props $ negate con of
      (Yes, evidence) -> force (negate ant) evidence props
      _ -> return props
  where
    force :: Monad m => Formula PropertyId -> Set PropertyId -> Properties -> LogicT m Properties
    force (Atom pid asserted) evidence props =
      case M.lookup pid props of
        Nothing -> addTrait sid pid asserted (Deduced (S.singleton tid) evidence) props
        Just found -> if found == asserted
          then return props
          else fatal $ Contradiction sid pid asserted found
    force (And sf) evidence props = foldM (\ps f -> force f evidence ps) props sf
    force (Or  sf) evidence props = do
      let subs     = map (\f -> (f, evaluate props f)) sf
          yeses    = [f | (f, (    Yes, _)) <- subs]
          unknowns = [f | (f, (Unknown, _)) <- subs]
          extra    = unionN [ev | (_, (No, ev)) <- subs]
      case (yeses, unknowns) of
        ([], [unknown]) -> force unknown (evidence `S.union` extra) props
        _ -> return props

addTrait :: Monad m => SpaceId -> PropertyId -> TVal -> Evidence -> Properties -> LogicT m Properties
addTrait s p v evidence props = do
  proverDeductions %= recordProof (s,p) v evidence
  return $ M.insert p v props

recordTheorem :: Theorem PropertyId -> Deductions -> Deductions
recordTheorem t r = r { deductionTheorems = t : deductionTheorems r }

recordProof :: TraitId -> TVal -> Evidence -> Deductions -> Deductions
recordProof tid value evidence r = r { deductionTraits = M.insert tid (value, evidence) $ deductionTraits r }

contains :: SpaceId -> PropertyId -> View -> Bool
contains sid pid = M.member pid . attributes sid

attributes :: SpaceId -> View -> Properties
attributes sid View{..} = case M.lookup sid _viewTraits of
  Nothing    -> M.empty
  Just props -> M.map _traitValue props

filterMatch :: Match -> [(Match, a)] -> Maybe a
filterMatch t pairs = listToMaybe [ts | (m, ts) <- pairs, m == t]

relatedTheorems :: Monad m => PropertyId -> LogicT m [(TheoremId, Implication PropertyId)]
relatedTheorems p = do
  relatedTheoremIds <- uses proverRelatedTheorems $ M.findWithDefault [] p
  allTheorems       <- use proverTheorems
  let
    add tid acc = case M.lookup tid allTheorems of
      Just impl -> (tid, impl) : acc
      Nothing   -> acc
  return $ foldr add [] relatedTheoremIds

addRelatedTheorem :: M.Map PropertyId [TheoremId] -> (TheoremId, Implication PropertyId) -> Map PropertyId [TheoremId]
addRelatedTheorem m (tid, impl) = S.foldr' (\p -> M.insertWith (++) p [tid]) m $ implicationProperties impl
