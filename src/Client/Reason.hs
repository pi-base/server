{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Client.Reason
  ( code
  ) where

import Server.Import

import qualified Data.Text       as Text
import qualified Data.Trait      as Trait
import qualified Data.User       as User
import qualified Data.UUID       as UUID
import           Reason          (ReasonType(..), toReasonDecoderSource, toReasonTypeSource)
import           Server.Api      (API)
import           Servant.Foreign (HasForeign(..), HasForeignType(..), Arg(..), HeaderArg(..), PathSegment(..), _reqHeaders)
import           Servant.Reason  (defReasonImports, generateReasonForAPI)

instance ReasonType (Formula PropertyId)
instance ReasonType (Implication PropertyId)
instance ReasonType Branch
instance ReasonType Citation
instance ReasonType CitationType
instance ReasonType CreatePropertyBody
instance ReasonType CreateSpaceBody
instance ReasonType CreateTheoremBody
instance ReasonType CreateTraitBody
instance ReasonType Property
instance ReasonType PullRequest
instance ReasonType PullRequestError
instance ReasonType Space
instance ReasonType Status
instance ReasonType Theorem
instance ReasonType Trait
instance ReasonType Trait.Value
instance ReasonType UpdatePropertyBody
instance ReasonType UpdateSpaceBody
instance ReasonType UpdateTheoremBody
instance ReasonType UpdateTraitBody
instance ReasonType User
instance ReasonType Version
instance ReasonType View

-- FIXME: this compiles, but probably produces incorrect client code
instance ReasonType UUID where
  toReasonType uuid = toReasonType $ UUID.toText uuid
instance ReasonType PropertyId where
  toReasonType id = toReasonType (show id :: Text)
instance ReasonType SpaceId where
  toReasonType id = toReasonType (show id :: Text)
instance ReasonType TheoremId where
  toReasonType id = toReasonType (show id :: Text)
instance ReasonType User.Id where
  toReasonType id = toReasonType (show id :: Text)
instance ReasonType (Either PullRequestError PullRequest) where
  toReasonType r  = toReasonType (show r :: Text)

-- Adapted from https://github.com/haskell-servant/servant/issues/672
-- Hopefully this'll land upstream before long
instance forall lang ftype api auths. ( HasForeign lang ftype api
                                        , HasForeignType lang ftype Text
         ) =>
         HasForeign lang ftype (AuthProtect auths :> api) where
  type Foreign ftype (AuthProtect auths
                      :> api) = Foreign ftype api
  foreignFor lang Proxy Proxy subR =
    foreignFor lang Proxy (Proxy :: Proxy api) req
    where
      req = subR {_reqHeaders = ReplaceHeaderArg arg "Bearer {Authorization}" : _reqHeaders subR}
      arg =
        Arg
          { _argName = PathSegment "Authorization"
          , _argType =
              typeFor lang (Proxy :: Proxy ftype) (Proxy :: Proxy Text)
          }

code :: Text
code = Text.intercalate "\n\n" $ defReasonImports
     --
     : toReasonDecoderSource (Proxy :: Proxy (Formula PropertyId))
     : toReasonDecoderSource (Proxy :: Proxy (Implication PropertyId))
     : toReasonDecoderSource (Proxy :: Proxy Branch)
     : toReasonDecoderSource (Proxy :: Proxy Citation)
     : toReasonDecoderSource (Proxy :: Proxy CitationType)
     : toReasonDecoderSource (Proxy :: Proxy CreatePropertyBody)
     : toReasonDecoderSource (Proxy :: Proxy CreateSpaceBody)
     : toReasonDecoderSource (Proxy :: Proxy CreateTheoremBody)
     : toReasonDecoderSource (Proxy :: Proxy CreateTraitBody)
     : toReasonDecoderSource (Proxy :: Proxy Property)
     : toReasonDecoderSource (Proxy :: Proxy PullRequest)
     : toReasonDecoderSource (Proxy :: Proxy PullRequestError)
     : toReasonDecoderSource (Proxy :: Proxy Space)
     : toReasonDecoderSource (Proxy :: Proxy Status)
     : toReasonDecoderSource (Proxy :: Proxy Theorem)
     : toReasonDecoderSource (Proxy :: Proxy Trait.Value)
     : toReasonDecoderSource (Proxy :: Proxy Trait)
     : toReasonDecoderSource (Proxy :: Proxy UpdatePropertyBody)
     : toReasonDecoderSource (Proxy :: Proxy UpdateSpaceBody)
     : toReasonDecoderSource (Proxy :: Proxy UpdateTheoremBody)
     : toReasonDecoderSource (Proxy :: Proxy UpdateTraitBody)
     : toReasonDecoderSource (Proxy :: Proxy User)
     : toReasonDecoderSource (Proxy :: Proxy Version)
     : toReasonDecoderSource (Proxy :: Proxy View)
     --  : toReasonDecoderSource (Proxy :: Proxy UUID)
     --  : toReasonDecoderSource (Proxy :: Proxy PropertyId)
     --  : toReasonDecoderSource (Proxy :: Proxy SpaceId)
     --  : toReasonDecoderSource (Proxy :: Proxy TheoremId)
     --  : toReasonDecoderSource (Proxy :: Proxy UserId)
     --
     : toReasonTypeSource    (Proxy :: Proxy (Formula PropertyId))
     : toReasonTypeSource    (Proxy :: Proxy (Implication PropertyId))
     : toReasonTypeSource    (Proxy :: Proxy Branch)
     : toReasonTypeSource    (Proxy :: Proxy Citation)
     : toReasonTypeSource    (Proxy :: Proxy CitationType)
     : toReasonTypeSource    (Proxy :: Proxy CreatePropertyBody)
     : toReasonTypeSource    (Proxy :: Proxy CreateSpaceBody)
     : toReasonTypeSource    (Proxy :: Proxy CreateTheoremBody)
     : toReasonTypeSource    (Proxy :: Proxy CreateTraitBody)
     : toReasonTypeSource    (Proxy :: Proxy Property)
     : toReasonTypeSource    (Proxy :: Proxy PullRequest)
     : toReasonTypeSource    (Proxy :: Proxy PullRequestError)
     : toReasonTypeSource    (Proxy :: Proxy Space)
     : toReasonTypeSource    (Proxy :: Proxy Status)
     : toReasonTypeSource    (Proxy :: Proxy Theorem)
     : toReasonTypeSource    (Proxy :: Proxy Trait.Value)
     : toReasonTypeSource    (Proxy :: Proxy Trait)
     : toReasonTypeSource    (Proxy :: Proxy UpdatePropertyBody)
     : toReasonTypeSource    (Proxy :: Proxy UpdateSpaceBody)
     : toReasonTypeSource    (Proxy :: Proxy UpdateTheoremBody)
     : toReasonTypeSource    (Proxy :: Proxy UpdateTraitBody)
     : toReasonTypeSource    (Proxy :: Proxy User)
     : toReasonTypeSource    (Proxy :: Proxy Version)
     : toReasonTypeSource    (Proxy :: Proxy View)
     --  : toReasonTypeSource    (Proxy :: Proxy UUID)
     --  : toReasonTypeSource    (Proxy :: Proxy PropertyId)
     --  : toReasonTypeSource    (Proxy :: Proxy SpaceId)
     --  : toReasonTypeSource    (Proxy :: Proxy TheoremId)
     --  : toReasonTypeSource    (Proxy :: Proxy UserId)
     --
     : generateReasonForAPI  (Proxy :: Proxy API)
