{-# LANGUAGE TemplateHaskell #-}
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
import Control.Monad.State.Strict     (MonadState)
import Control.Monad.Trans.Except     (throwE)
import Control.Monad.Trans.RWS.Strict (RWST, runRWST)
import qualified Data.Map.Strict as M
import qualified Data.Set        as S

import           Core        hiding (force, negate)
import qualified Data.Loader as Loader
import           Formula     (negate)
import           Util        (unionN)

data Evidence = Asserted
              | Deduced (Set TheoremId) (Set PropertyId)
              deriving Show

newtype LogicT m a = LogicT
  { unLogicT :: ExceptT LogicError (RWST Loader.Loader () Prover m) a
  } deriving (Functor, Applicative, Monad, MonadState Prover, MonadReader Loader.Loader)

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

getLoader :: Monad m => LogicT m Loader.Loader
getLoader = LogicT $ lift $ ask

loadSpace :: MonadStore m => SpaceId -> LogicT m Properties
loadSpace _id = do
  loader <- getLoader
  load $ Loader.spaceTraits loader _id

-- FIXME: catch errors and re-raise as LoadFailures
load :: Monad m => m b -> LogicT m b
load f = lift f

loadEachSpace :: MonadStore m => ((SpaceId, Properties) -> LogicT m a) -> LogicT m ()
loadEachSpace f = do
  loader   <- getLoader
  spaceIds <- load $ Loader.spaceIds loader
  forM_ spaceIds $ \sid -> void $ do
    traceM $ "Loading space: " ++ show sid
    props <- load $ Loader.spaceTraits loader sid
    f (sid, props)

assertTrait :: MonadStore m => Trait SpaceId PropertyId -> LogicT m ()
assertTrait Trait{..} = do
  props <- loadSpace _traitSpace
  case M.lookup _traitProperty props of
    Just val -> unless (val == _traitValue) $
      fatal $ Contradiction _traitSpace _traitProperty _traitValue val
    Nothing -> do
      props'  <- addTrait _traitSpace _traitProperty _traitValue Asserted props
      related <- relatedTheorems _traitProperty
      void $ foldM (\ps (tid, impl) -> applyTheorem tid impl _traitSpace ps) props' related

assertTheorem :: MonadStore m => Theorem PropertyId -> LogicT m ()
assertTheorem t = do
  let tid  = theoremId t
      impl = theoremImplication t
  traceM "Theorems"
  proverTheorems        %= M.insert tid impl
  traceM "Related"
  proverRelatedTheorems %= flip addRelatedTheorem (tid, impl)
  traceM "Deductions"
  proverDeductions      %= recordTheorem t
  traceM "Spaces"
  loadEachSpace $ \(space, props) -> do
    traceM $ "Applying " ++ show space
    applyTheorem (theoremId t) (theoremImplication t) space props

checkAll :: MonadStore m => LogicT m ()
checkAll = do
  theorems <- use $ proverTheorems
  loadEachSpace $ \(space, props) -> do
    foldM (\ps (tid, impl) -> applyTheorem tid impl space ps) props $ M.toList theorems

runLogicT :: MonadStore m => Loader.Loader -> LogicT m a -> m (Either LogicError Deductions)
runLogicT loader handler = do
  theorems <- Loader.implications loader
  traceM $ show theorems
  let rwst = runExceptT $ unLogicT handler
  traceM "Initializing prover"
  (result, prover, ()) <- runRWST rwst loader $ initializeProver theorems
  traceM "Got result"
  traceM $ show $ _proverDeductions prover
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

applyTheorem :: Monad m => TheoremId -> Implication PropertyId -> SpaceId -> Properties -> LogicT m Properties
applyTheorem tid (Implication ant con) sid props =
  trace ("Applying: " ++ show (Implication ant con)) $
  case evaluate props ant of
    (No, _)         -> return props
    (Yes, evidence) -> force con evidence props
    (Unknown, _)    -> case evaluate props $ negate con of
      (Yes, evidence) -> force (negate ant) evidence props
      _ -> return props
  where
    force :: Monad m => Formula PropertyId -> Set PropertyId -> Properties -> LogicT m Properties
    force (Atom pid asserted) evidence props' =
      case M.lookup pid props' of
        Nothing -> addTrait sid pid asserted (Deduced (S.singleton tid) evidence) props'
        Just found -> if found == asserted
          then return props'
          else fatal $ Contradiction sid pid asserted found
    force (And sf) evidence props' = foldM (\ps f -> force f evidence ps) props' sf
    force (Or  sf) evidence props' = do
      let subs     = map (\f -> (f, evaluate props' f)) sf
          yeses    = [f | (f, (    Yes, _)) <- subs]
          unknowns = [f | (f, (Unknown, _)) <- subs]
          extra    = unionN [ev | (_, (No, ev)) <- subs]
      case (yeses, unknowns) of
        ([], [unknown]) -> force unknown (evidence `S.union` extra) props'
        _ -> return props'

addTrait :: Monad m => SpaceId -> PropertyId -> TVal -> Evidence -> Properties -> LogicT m Properties
addTrait s p v evidence props = do
  proverDeductions %= recordProof (s,p) v evidence
  return $ M.insert p v props

recordTheorem :: Theorem PropertyId -> Deductions -> Deductions
recordTheorem t r = r { deductionTheorems = t : deductionTheorems r }

recordProof :: TraitId -> TVal -> Evidence -> Deductions -> Deductions
recordProof tid value evidence r = r { deductionTraits = M.insert tid (value, evidence) $ deductionTraits r }

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
