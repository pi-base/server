{-# OPTIONS_GHC -fno-warn-orphans #-}
module Class where

import Import

import           Control.Monad.Fail     (MonadFail(..))
import           Data.Aeson             (ToJSON(..), FromJSON(..), object, withObject, (.=), (.:))
import           Data.Aeson.Types       (Parser)

import           Data.Citation    (Citation(..), CitationType(..))
import           Data.Id          as Id (decode, encode)
import           Data.Property    as Property (PropertyId)
import           Data.Formula     as Formula (Formula)

instance ToJSON Citation where
  toJSON Citation{..} =
    let type' = case citationType of
          DOICitation  -> "doi"
          MRCitation   -> "mr"
          WikiCitation -> "wikipedia"
    in object [ type' .= citationRef, ("name" :: Text) .= citationName ]

instance FromJSON Citation where
  parseJSON = withObject "Citation" $ \c -> do
    citationName <- c .: "name"
    (citationType, citationRef) <- getRef c "doi" DOICitation
                               <|> getRef c "mr" MRCitation
                               <|> getRef c "wikipedia" WikiCitation
    return Citation{..}
    where
      getRef c text type' = do
        citationRef <- c .: text
        return $ (type', citationRef)

instance ToJSON (Formula PropertyId) where
  toJSON f = toJSON $ Id.encode <$> f

-- TODO: validate that these are well-formed ids
-- and fail parsing if not
instance FromJSON (Formula PropertyId) where
  parseJSON raw = do
    tFormula <- (parseJSON raw :: Parser (Formula Text))
    case sequence $ fmap Id.decode tFormula of
      Right f -> return f
      _ -> fail "failed to parse Formula PropertyId"
