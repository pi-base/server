{-# OPTIONS_GHC -fno-warn-orphans #-}
module Data.Formula
  ( Formula(..)
  , (.=)
  , (/\)
  , (\/)
  , format
  , negate
  , parse
  , properties
  ) where

import Import hiding (intercalate, negate, option)

import           Control.Monad        (void, mzero)
import           Data.Aeson           hiding ((.=))
import qualified Data.Aeson           as A
import           Data.Attoparsec.Text hiding (parse)
import qualified Data.HashMap.Strict  as HM
import           Data.Monoid          ((<>))
import qualified Data.Set             as S
import           Data.String          (IsString, String)
import           Data.Text            (Text, intercalate, strip)
import           Data.Trait           as Trait (Value(..))

data Formula p = Atom p Trait.Value
               | And [Formula p]
               | Or  [Formula p]
               deriving (Generic, Eq, Functor, Foldable, Show, Traversable)

enclose :: (IsString s, Monoid s) => s -> s
enclose s = "(" <> s <> ")"

format :: (p -> Text) -> Formula p -> Text
format f (Atom p (Trait.Value True )) = f p
format f (Atom p (Trait.Value False)) = "~" <> f p
format f (And fs) = enclose . intercalate " + " $ map (format f) fs
format f (Or  fs) = enclose . intercalate " | " $ map (format f) fs

(.=) :: p -> Bool -> Formula p
(.=) p = Atom p . Trait.Value
infixl 5 .=

(/\) :: Formula p -> Formula p -> Formula p
(/\) f (And fs) = And $ f : fs
(/\) (And fs) f = And $ fs ++ [f]
(/\) f g = And [f,g]
infixl 4 /\

(\/) :: Formula p -> Formula p -> Formula p
(\/) f (Or fs) = Or $ f : fs
(\/) (Or fs) f = Or $ fs ++ [f]
(\/) f g = Or [f,g]
infixl 4 \/

negate :: Formula p -> Formula p
negate (Atom p (Trait.Value v)) = Atom p (Trait.Value $ not v)
negate (And  fs) = Or  $ map negate fs
negate (Or   fs) = And $ map negate fs

parse :: Text -> Either String (Formula Text)
parse t =
  let parser = formulaP <* endOfInput
  in case parseOnly parser t of
    Left err -> case parseOnly parser (enclose t) of
      Left _ -> Left err
      r -> r
    r -> r

whitespace :: Parser ()
whitespace = void . many' . satisfy $ inClass [' ', '\t', '\n']

formulaP :: Parser (Formula Text)
formulaP = choice [conjP, disjP, atomP]

compoundP :: Parser Text
          -> ([Formula Text] -> Formula Text)
          -> Parser (Formula Text)
compoundP separator constructor = do
  fs <- "(" *> pad (formulaP `sepBy1` (pad separator)) <* ")"
  return $ constructor fs

pad :: Parser a -> Parser a
pad parser = whitespace *> parser <* whitespace

conjP :: Parser (Formula Text)
conjP = compoundP "+" And

disjP :: Parser (Formula Text)
disjP = compoundP "|" Or

atomP :: Parser (Formula Text)
atomP = do
  mode  <- option "" $ choice ["not ", "-", "~"]
  label <- takeTill $ inClass ['+', '|', ')']
  return $ Atom (strip label) (Trait.Value $ mode == "")

instance FromJSON (Formula Text) where
  parseJSON (Object v) = case head . HM.toList $ v of
    Just ("and", val)  -> And <$> parseJSON val
    Just ("or", val)   -> Or <$> parseJSON val
    Just (slug, Bool b) -> return $ Atom slug $ Trait.Value b
    _ -> mzero
  parseJSON _ = mzero

instance ToJSON (Formula Text) where
  toJSON (And subs) = object [ "and" A..= toJSON subs ]
  toJSON (Or subs) = object [ "or"  A..= toJSON subs ]
  toJSON (Atom p v)  = object [ p A..= v ]

properties :: (Ord a) => Formula a -> S.Set a
properties (Atom p _) = S.singleton p
properties (And sf ) = unionN $ map properties sf
properties (Or sf ) = unionN $ map properties sf

unionN :: Ord a => [S.Set a] -> S.Set a
unionN = foldl' S.union S.empty
