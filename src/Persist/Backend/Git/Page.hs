{-# LANGUAGE MultiParamTypeClasses #-}
module Persist.Backend.Git.Page
  ( Page
  , Pagable
  , Data'(..)
  , Data
  , Field(..)
  , id
  , page
  , parse
  , parse'
  , write
  , write'
  , header
  , header'
  , main
  , section
  ) where

import Core

import           Control.Lens                  (Lens', lens, set, view)
import           Core                          (ParseError(..))
import           Data.Aeson                    (FromJSON(..), ToJSON(..))
import           Data.Aeson.Types              (parseMaybe)
import qualified Data.HashMap.Strict           as HM
import           GHC.Generics                  (from, to)
import           Persist.Backend.Git.Page.Data (Data'(..), Data)
import qualified Persist.Backend.Git.Page.Data as Page

page :: Lens' (a Identity) id
     -> a Field
     -> Page a id
page = Page

data Page a id = Page
  { id     :: Lens' (a Identity) id
  , fields :: a Field
  }

newtype Field a = Field
  { unField :: Lens' Data (Maybe a)
  }

class
  ( Generic (a Field)
  , Generic (a Identity)
  , GWrite (Rep (a Field))
           (Rep (a Identity))
  , GParse (Rep (a Field))
           (Rep (a Identity))
  ) => Pagable a

class GWrite f i where
  gwrite :: f p -> i p -> Data -> Data

transform ::
  ( Generic (f Field)
  , Generic (f Identity)
  , GWrite (Rep (f Field))
           (Rep (f Identity))
  )
  => f Field
  -> f Identity
  -> Data
  -> Data
transform fields dat = gwrite (from fields) (from dat)

instance GWrite (K1 a (Field k)) (K1 a k) where
  gwrite (K1 (Field field)) (K1 value) = set field (Just value)
  {-# INLINE gwrite #-}

instance (GWrite f i, GWrite f' i')
  => GWrite (f :*: f') (i :*: i') where
  gwrite (f :*: f') (i :*: i') = gwrite f i . gwrite f' i'
  {-# INLINE gwrite #-}

instance GWrite f i
  => GWrite (M1 _a _b f) (M1 _a' _b' i) where
  gwrite (M1 f) (M1 i) = gwrite f i
  {-# INLINE gwrite #-}

write' ::
  ( Generic (a Field)
  , Generic (a Identity)
  , GWrite (Rep (a Field))
           (Rep (a Identity))
  )
  => a Field -> a Identity -> Data
write' fields obj = transform fields obj
  $ Data mempty mempty mempty

class GParse f i where
  gparse :: f p -> Data -> Maybe (i p)

instance GParse (K1 a (Field k)) (K1 a k) where
  gparse (K1 (Field l)) p = K1 <$> view l p
  {-# INLINE gparse #-}

instance (GParse f i, GParse f' i')
  => GParse (f :*: f') (i :*: i') where
  gparse (f :*: f') p = (:*:)
    <$> gparse f p
    <*> gparse f' p
  {-# INLINE gparse #-}

instance GParse f i
  => GParse (M1 _a _b f) (M1 _a' _b' i) where
  gparse (M1 f) p = M1 <$> gparse f p
  {-# INLINE gparse #-}

parse' ::
  ( Generic (f Field)
  , Generic (f Identity)
  , GParse (Rep (f Field))
           (Rep (f Identity))
  )
  => f Field
  -> Data
  -> Maybe (f Identity)
parse' fields p = to <$> gparse (from fields) p

parse :: Pagable a
      => Page a i
      -> Text
      -> Either ParseError (a Identity)
parse p content = do
  sheet <- Page.parse content
  case parse' (fields p) sheet of
    Just a  -> return a
    Nothing -> Left $ ParseError "does not define a valid object"

write :: Pagable a
      => Page a i
      -> (a Identity)
      -> Text
write p object = Page.write $ write' (fields p) object

-- TODO: simplify these Field definitions using the new Data lenses

header :: (FromJSON a, ToJSON a) => Text -> Field a
header name = Field $ lens getter setter
  where
    getter :: FromJSON a => Data -> Maybe a
    getter Data{..} = HM.lookup name _headers >>= parseMaybe parseJSON

    setter :: ToJSON a => Data -> Maybe a -> Data
    setter sheet mvalue = sheet
      { _headers =
          case mvalue of
            Just value -> HM.insert name (toJSON value) $ _headers sheet
            _ -> HM.delete name $ _headers sheet
      }

header' :: (FromJSON a, ToJSON a, Monoid a) => Text -> Field a
header' name = Field $ lens getter setter
  where
    getter :: (FromJSON a, Monoid a) => Data -> Maybe a
    getter Data{..} =
      let found = HM.lookup name _headers >>= parseMaybe parseJSON
      in  Just $ fromMaybe mempty found

    setter :: ToJSON a => Data -> Maybe a -> Data
    setter sheet value = sheet { _headers = HM.insert name (toJSON value) $ _headers sheet }

main :: Field Text
main = Field $ lens getter setter
  where
    getter Data{..} = Just _main
    setter sheet value = sheet { _main = fromMaybe "" value }

section :: Text -> Field (Maybe Text)
section name = Field $ lens getter setter
  where
    getter :: Data -> Maybe (Maybe Text)
    getter Data{..} = Just $ HM.lookup name _sections

    setter :: Data -> Maybe (Maybe Text) -> Data
    setter sheet mvalue = sheet
      { _sections =
          case join mvalue of
            Just value -> HM.insert name value $ _sections sheet
            _ -> HM.delete name $ _sections sheet
      }
