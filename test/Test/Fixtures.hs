module Test.Fixtures where

import           Core
import           Data.Formula     ((.=))
import qualified Data.Id          as Id
import           Data.Implication ((~>))
import qualified Data.Property    as Property (id)
import qualified Data.Space       as Space    (id)
import qualified Data.Trait       as Trait    (Value(..))
import           Persist.Store    (Action(..))

property :: Int -> Text -> Property
property id name = Property
  (Id.fromInt id)
  name
  []
  ""
  []

space :: Int -> Text -> Space
space id name = Space
  (Id.fromInt id)
  name
  []
  ""
  Nothing
  []

counterexamplesInTopology :: Citation
counterexamplesInTopology = Citation "Counterexamples in Topology" DOICitation "10.1007/978-1-4612-6290-9"

compact :: Property
compact = Property
  (Id.fromInt 16)
  "Compact"
  []
  "Every open cover has a finite subcover"
  []

countablyCompact :: Property
countablyCompact = Property
  (Id.fromInt 19)
  "Countably Compact"
  []
  "Every countable open cover has a finite subcover"
  []

connected :: Property
connected = property 2 "Connected"

t2 :: Property
t2 = property 3 "T_2"

finiteDiscrete :: Space
finiteDiscrete = Space
  (Id.fromInt 1)
  "Finite Discrete Topology"
  []
  "Every set open"
  (Just "Trivially")
  []

finiteDiscreteIsCompact :: Trait
finiteDiscreteIsCompact = Trait
  (Space.id finiteDiscrete)
  (Property.id compact)
  (Trait.Value True)
  "Trivially"
  []

compactIsCountablyCompact :: Theorem
compactIsCountablyCompact = Theorem
  (Id.fromInt 1)
  (fmap Property.id (compact .= True ~> countablyCompact .= True))
  Nothing
  "Trivially"
  [ counterexamplesInTopology ]

james :: User
james = User "James" "james@pi-base.org" True

steven :: User
steven = User "Steven" "steven@pi-base.org" True

rachel :: User
rachel = User "Rachel" "rachel@pi-base.org" True

leah :: User
leah = User "Leah" "leah@pi-base.org" False

master :: Branch
master = Branch "master"

test :: Branch
test = Branch "test"

setup :: Action
setup = Message "Setup test"
