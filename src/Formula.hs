{-# LANGUAGE DeriveFunctor #-}
module Formula
  ( Formula(..)
  , (.=)
  , (.+)
  , (.|)
  , format
  , negate
  , parse
  , hydrate
  ) where

import Prelude hiding (negate)

import Control.Monad (void, mzero)
import Data.Aeson hiding ((.=))
import qualified Data.Aeson as A
import Data.Attoparsec.Text hiding (parse)
import qualified Data.HashMap.Strict as HM
import Data.List (intercalate)
import qualified Data.Map.Strict as M
import Data.Monoid ((<>))
import Data.String (IsString)
import Data.Text (Text, strip, unpack)

data Formula p = Atom p Bool
               | Conj [Formula p]
               | Disj [Formula p]
               deriving (Eq, Functor)

enclose :: (IsString s, Monoid s) => s -> s
enclose s = "(" <> s <> ")"

format :: (p -> String) -> Formula p -> String
format f (Atom p True) = f p
format f (Atom p False) = "~" ++ f p
format f (Conj fs)  = enclose . intercalate " + " $ map (format f) fs
format f (Disj fs)  = enclose . intercalate " | " $ map (format f) fs

instance Show (Formula Text) where
  show = format unpack

(.=) :: p -> Bool -> Formula p
(.=) = Atom
infixl 5 .=

(.+) :: Formula p -> Formula p -> Formula p
(.+) f (Conj fs) = Conj $ f : fs
(.+) (Conj fs) f = Conj $ fs ++ [f]
(.+) f g = Conj [f,g]
infixl 4 .+

(.|) :: Formula p -> Formula p -> Formula p
(.|) f (Disj fs) = Disj $ f : fs
(.|) (Disj fs) f = Disj $ fs ++ [f]
(.|) f g = Disj [f,g]
infixl 4 .|

negate :: Formula p -> Formula p
negate (Atom p v) = Atom p (not v)
negate (Conj fs) = Disj $ map negate fs
negate (Disj fs) = Conj $ map negate fs

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
conjP = compoundP "+" Conj

disjP :: Parser (Formula Text)
disjP = compoundP "|" Disj

atomP :: Parser (Formula Text)
atomP = do
  mode  <- option "" $ choice ["not ", "-", "~"]
  label <- takeTill $ inClass ['+', '|', ')']
  return $ Atom (strip label) (mode == "")

instance FromJSON (Formula Text) where
  parseJSON (Object v) = case head . HM.toList $ v of
    ("and", val)  -> Conj <$> parseJSON val
    ("or", val)   -> Disj <$> parseJSON val
    (slug, Bool b) -> return $ Atom slug b
    _ -> mzero
  parseJSON _ = mzero

instance ToJSON (Formula Text) where
  toJSON (Conj subs) = object [ "and" A..= toJSON subs ]
  toJSON (Disj subs) = object [ "or"  A..= toJSON subs ]
  toJSON (Atom p v)  = object [ p A..= v ]

-- TODO: these definitely could be cleaner
hydrate :: Ord a => M.Map a b -> Formula a -> Either [a] (Formula b)
hydrate props (Atom p v) = case M.lookup p props of
  Just p' -> Right $ Atom p' v
  Nothing -> Left [p]
hydrate props (Conj subs) = allRight Conj $ map (hydrate props) subs
hydrate props (Disj subs) = allRight Disj $ map (hydrate props) subs

allRight :: ([f] -> r) -> [Either [a] f] -> Either [a] r
allRight constructor subs = case foldl step (Right []) subs of
  Left as  -> Left as
  Right fs -> Right $ constructor fs
  where
    step :: Either [a] [f] -> Either [a] f -> Either [a] [f]
    step (Left as) (Left bs) = Left $ as ++ bs
    step (Left as) _ = Left as
    step _ (Left bs) = Left bs
    step (Right fs) (Right f) = Right $ f : fs
