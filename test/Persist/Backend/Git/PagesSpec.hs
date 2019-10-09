module Persist.Backend.Git.PagesSpec (spec) where

import           Test.Import
import qualified Test.QuickCheck as Q
import qualified Test.Fixtures as F

import Data.Text                 (unlines)
import Persist.Backend.Git.Page  (Page, Pagable, parse, write)
import Persist.Backend.Git.Pages (property, space, theorem, trait)

spec :: IO TestTree
spec = testSpec "Persist.Backend.Git.Pages" $ parallel $ do
  describe "space" $ do
    isPage space

    parses
      space
      F.finiteDiscrete
      [r|---
         uid: S000001
         name: Finite Discrete Topology
         ---
         Every set open
         [[Proof of Topology]]
         Trivially
      |]

    translates
      space
      F.finiteDiscrete
      ( unlines
        [ "---"
        , "uid: S000001"
        , "aliases: []"
        , "name: Finite Discrete Topology"
        , "refs: []"
        , "---"
        , "Every set open"
        , "[[Proof of Topology]]"
        , "Trivially"
        ]
      )

  describe "property" $ do
    isPage property

    parses
      property
      F.compact
      [r|---
         uid: P000016
         name: Compact
         ---
         Every open cover has a finite subcover
      |]

    translates
      property
      F.compact
      ( unlines
        [ "---"
        , "uid: P000016"
        , "aliases: []"
        , "name: Compact"
        , "refs: []"
        , "---"
        , "Every open cover has a finite subcover"
        ]
      )

  describe "trait" $ do
    isPage trait

    parses
      trait
      F.finiteDiscreteIsCompact
      [r|---
         uid: T000008
         space: S000001
         property: P000016
         value: true
         ---
         Trivially
      |]

  describe "theorem" $ do
    isPage theorem

    parses
      theorem
      F.compactIsCountablyCompact
      [r|---
         uid: T000001
         if:
           P000016: true
         then:
           P000019: true
         refs:
           - doi: 10.1007/978-1-4612-6290-9
             name: Counterexamples in Topology
         ---
         Trivially
      |]

    translates
      theorem
      F.compactIsCountablyCompact
      ( unlines
        [ "---"
        , "if:"
        , "  P000016: true"
        , "converse: null"
        , "uid: T000001"
        , "then:"
        , "  P000019: true"
        , "refs:"
        , "- doi: 10.1007/978-1-4612-6290-9"
        , "  name: Counterexamples in Topology"
        , "---"
        , "Trivially"
        ]
      )

parses :: (Show (a Identity), Eq (a Identity), Pagable a)
       => Page a i -> (a Identity) -> Text -> Spec
parses page expected body =
  it "can parse" $ do
    parse page body `shouldBe` Right expected

writes :: (Show (a Identity), Eq (a Identity), Pagable a)
       => Page a i -> (a Identity) -> Text -> Spec
writes page object body =
  it "can write" $ do
    write page object `shouldBe` body


translates :: (Show (a Identity), Eq (a Identity), Pagable a)
           => Page a i -> (a Identity) -> Text -> Spec
translates page object body =
  describe "translating" $ do
    parses page object body
    writes page object body

isPage :: (Eq (a Identity), Show (a Identity), Arbitrary (a Identity), Pagable a)
       => Page a i -> Spec
isPage page = describe "page" $ do
  it "is invertable" $ Q.property $ \obj ->
    (parse page $ write page obj) == Right obj
