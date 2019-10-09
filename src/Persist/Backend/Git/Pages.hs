{-# OPTIONS_GHC -fno-warn-orphans #-}
module Persist.Backend.Git.Pages
  ( property
  , space
  , trait
  , theorem
  ) where

import Core

import           Control.Lens                  hiding ((.=))
import qualified Data.Aeson                    as Aeson
import qualified Data.Aeson.Types              as Aeson
import qualified Data.HashMap.Strict           as HM
import qualified Data.Property                 as Property
import qualified Data.Space                    as Space
import qualified Data.Theorem                  as Theorem
import qualified Data.Trait                    as Trait
import qualified Persist.Backend.Git.Page.Data as Data (Data'(..), Data)
import           Persist.Backend.Git.Page      (Field(..), Pagable, Page, page, header, header', main, section)

instance Pagable Space'
instance Pagable Property'
instance Pagable Theorem'
instance Pagable Trait'

space :: Page Space' SpaceId
space = page Space.idL
  ( Space
    (header  "uid")
    (header  "name")
    (header' "aliases")
    main
    (section "Proof of Topology")
    (header' "refs")
  )

property :: Page Property' PropertyId
property = page Property.idL
  ( Property
    (header "uid")
    (header "name")
    (header' "aliases")
    main
    (header' "refs")
  )

trait :: Page Trait' TraitId
trait = page Trait.idL
  ( Trait
    (header "space")
    (header "property")
    (header "value")
    main
    (header' "refs")
  )

theorem :: Page Theorem' TheoremId
theorem = page Theorem.idL
  ( Theorem
    (header "uid")
    implicationHeaders
    (header' "converse")
    main
    (header' "refs")
  )

implicationHeaders :: Field (Implication PropertyId)
implicationHeaders = Field $ lens getter' setter
  where
    getter' :: Data.Data -> Maybe (Implication PropertyId)
    getter' Data.Data{..} = Implication
      <$> (HM.lookup "if"   _headers >>= Aeson.parseMaybe Aeson.parseJSON)
      <*> (HM.lookup "then" _headers >>= Aeson.parseMaybe Aeson.parseJSON)

    setter :: Data.Data -> Maybe (Implication PropertyId) -> Data.Data
    setter sheet@Data.Data{..} mi =
      sheet { Data._headers = case mi of
        Just (Implication ant con) -> HM.insert "if" (Aeson.toJSON ant) $ HM.insert "then" (Aeson.toJSON con) $ Data._headers sheet
        _ -> HM.delete "if" $ HM.delete "then" $ Data._headers sheet
      }
