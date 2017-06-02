module Logic.Universe
  ( Universe
  , Evidence
  , attributes
  , check
  , contains
  , fromViewer
  , insertTheorem
  , insertTrait
  , Logic.Universe.lookup
  , relevantTheorems
  , search
  , withQueue
  ) where

import           Control.Monad.State.Strict (State, execState, modify, gets, get, put)
import qualified Data.Map.Strict as M
import qualified Data.Set as S

import Core    hiding (empty)
import Formula (Formula(..))
import Util    (indexBy, unionN)
import Viewer  hiding (empty)

type Properties = M.Map PropertyId Bool
type Evidence = (TheoremId, S.Set PropertyId)

data Universe = Universe
  { uSpaces        :: M.Map SpaceId Space
  , uProperties    :: M.Map PropertyId Property
  , uTraits        :: M.Map TraitId (Trait SpaceId PropertyId)
  , uTheorems      :: M.Map TheoremId (Implication PropertyId)
  , uProofs        :: M.Map TraitId Assumptions
  , uSpaceIndex    :: M.Map SpaceId (M.Map PropertyId TraitId)
  , uPropertyIndex :: M.Map PropertyId (M.Map SpaceId TraitId)
  , uRelTheorems   :: M.Map PropertyId [TheoremId]
  , uQueue         :: S.Set (SpaceId, PropertyId)
  , uId            :: Int
  } deriving Show

fromViewer :: Viewer -> Universe
fromViewer Viewer{..} = flip execState initial $ do
  forM_ viewerTheorems $ \t ->
    insertTheorem (Just $ theoremId t) (map propertyId $ theoremImplication t)

  forM_ viewerTraits $ \Trait{..} ->
    insertTrait (Just traitId) (spaceId traitSpace) (propertyId traitProperty) traitValue Nothing

  where
    initial = empty
      { uSpaces     = indexBy spaceId    viewerSpaces
      , uProperties = indexBy propertyId viewerProperties
      }

empty :: Universe
empty = Universe
  { uSpaces        = mempty
  , uProperties    = mempty
  , uTraits        = mempty
  , uTheorems      = mempty
  , uProofs        = mempty
  , uSpaceIndex    = mempty
  , uPropertyIndex = mempty
  , uRelTheorems   = mempty
  , uQueue         = mempty
  , uId            = 1
  }

lookup :: SpaceId -> PropertyId -> Universe -> Maybe Bool
lookup s p = M.lookup p . attributes s

contains :: SpaceId -> PropertyId -> Universe -> Bool
contains sid pid = M.member pid . attributes sid

attributes :: SpaceId -> Universe -> Properties
attributes sid Universe{..} = case M.lookup sid uSpaceIndex of
  Nothing -> M.empty
  Just props -> M.mapMaybe (lookupTraitValue uTraits) props
  where
    lookupTraitValue traits tid = traitValue <$> M.lookup tid traits

relevantTheorems :: PropertyId -> Universe -> [(TheoremId, Implication PropertyId)]
relevantTheorems pid u = z $ map withImplication theoremIds
  where
    theoremIds = M.findWithDefault [] pid $ uRelTheorems u
    withImplication tid = (tid, M.lookup tid $ uTheorems u)

    z [] = []
    z ((x, Just y) : zs) = (x,y) : z zs
    z (          _ : zs) =         z zs

nextId :: State Universe Text
nextId = do
  modify $ \u -> u { uId = uId u + 1}
  _id <- gets uId
  return $ "x" <> tshow _id

insertTrait :: Maybe TraitId
            -> SpaceId
            -> PropertyId
            -> Bool
            -> Maybe Evidence
            -> State Universe ()
insertTrait mtid sid pid value mev = do
  tid <- maybe (TraitId <$> nextId) return mtid
  let trait = Trait tid sid pid value "" :: Trait SpaceId PropertyId
  modify $ \u -> u
    { uTraits        = M.insert tid trait $ uTraits u
    , uSpaceIndex    = addNested sid pid tid $ uSpaceIndex u
    , uPropertyIndex = addNested pid sid tid $ uPropertyIndex u
    , uProofs = case mev of
        Just ev -> M.insert tid (buildProof u trait ev) $ uProofs u
        Nothing -> uProofs u
    , uQueue = S.insert (sid, pid) $ uQueue u
    }

addNested :: (Ord k1, Ord k2)
          => k1 -> k2 -> val -> M.Map k1 (M.Map k2 val) -> M.Map k1 (M.Map k2 val)
addNested k1 k2 val m = M.insertWith M.union k1 (M.singleton k2 val) m

insertTheorem :: Maybe TheoremId -> Implication PropertyId -> State Universe ()
insertTheorem mtid i = do
  tid <- maybe (TheoremId <$> nextId) return mtid
  modify $ \u -> u
    { uTheorems    = M.insert tid i $ uTheorems u
    , uRelTheorems = foldl' (addRelated tid) (uRelTheorems u) (S.toList $ implicationProperties i)
    }
  queueTheorem i
  where
    addRelated :: TheoremId -> M.Map PropertyId [TheoremId] -> PropertyId -> M.Map PropertyId [TheoremId]
    addRelated tid m p = M.insertWith (++) p [tid] m

search :: Formula PropertyId -> Match -> Universe -> [SpaceId]
search f mode u = filter matches . M.keys $ uSpaces u
  where
    matches sid = mode == (fst $ check (attributes sid u) f)

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

filterMatch :: Match -> [(Match, a)] -> Maybe a
filterMatch t pairs = listToMaybe [ts | (m, ts) <- pairs, m == t]

queueTheorem :: Implication PropertyId -> State Universe ()
queueTheorem i@(Implication ant con) = do
  -- TODO: can tighten up which items are queued to check
  modify $ \u -> u { uQueue = toCheck u `S.union` uQueue u }
  where
    unknowns :: Universe -> [SpaceId]
    unknowns u = search (Or [ant, con]) Unknown u

    toCheck :: Universe -> S.Set (SpaceId, PropertyId)
    toCheck u = S.fromList [(s,p) | s <- unknowns u, p <- (S.toList $ implicationProperties i)]


queueNext :: State Universe (Maybe (SpaceId, PropertyId))
queueNext = do
  u <- get
  case S.maxView (uQueue u) of
    Just ((s,p), rest) -> do
      put $ u { uQueue = rest }
      return $ Just (s,p)
    Nothing -> return Nothing

runQueue :: (SpaceId -> PropertyId -> State Universe ()) -> State Universe ()
runQueue handler = queueNext >>= \case
  Just (s,p) -> do
    handler s p
    runQueue handler
  _ -> return ()

withQueue :: (SpaceId -> PropertyId -> State Universe ())
          -> Universe
          -> [(Trait Space Property, Assumptions)]
withQueue handler universe =
  let
    proofs = uProofs universe
    u = execState (runQueue handler) universe
    newProofs = uProofs u -- proofs `M.difference` uProofs u
  in catMaybes . map (expandTrait u) $ M.toList newProofs

expandTrait :: Universe -> (TraitId, a) -> Maybe (Trait Space Property, a)
expandTrait Universe{..} (tid, a) = do
  t <- M.lookup tid uTraits
  s <- M.lookup (traitSpace t) uSpaces
  p <- M.lookup (traitProperty t) uProperties
  return $ (t { traitSpace = s, traitProperty = p }, a)

fromJust :: Maybe a -> a
fromJust Nothing = error "fromJust Nothing"
fromJust (Just a) = a

buildProof :: Universe -> Trait SpaceId PropertyId -> Evidence -> Assumptions
buildProof Universe{..} Trait{..} (thrmId, props) =
  let traitIds = fromJust $ M.lookup traitSpace uSpaceIndex
  in Assumptions
    { assumedTraits   = S.map (\pid -> fromJust $ M.lookup pid traitIds) props
    , assumedTheorems = S.singleton thrmId
    }
