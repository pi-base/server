{-# LANGUAGE DuplicateRecordFields #-}
module Server.Api
  ( Server
  , API
  , context
  , server
  --
  , Auth.Forbidden(..)
  , Auth.LoginRequired(..)
  , NotFound(..)
  , branches
  , createSpace
  , me
  , root
  , status
  , theoremName
  , traitName
  , viewBranch
  ) where

import Server.Import
import Server.Class ()

import qualified Data.Branch                      as Branch
import qualified Data.Implication                 as Implication
import qualified Data.Map                         as Map
import qualified Data.Property                    as Property
import qualified Data.Space                       as Space
import qualified Data.Theorem                     as Theorem
import qualified Data.Trait                       as Trait
import qualified Persist.Properties               as Properties
import           Servant.Server.Experimental.Auth (AuthServerData, mkAuthHandler)

import Polysemy.Error  (Error, throw)
import Polysemy.Reader (Reader, ask)

import qualified Persist.Auth     as Auth
import qualified Persist.Branches as Branches
import qualified Persist.Spaces   as Spaces
import qualified Persist.Store    as Store

type GET   = Get   '[JSON]
type POST  = Post  '[JSON]
type PATCH = Patch '[JSON]

type Body = ReqBody '[JSON]

data NotFound = NotFound
  deriving (Show, Eq)

type API = AuthProtect "token-auth" :>
  ( GET View
  :<|> "branches" :> GET [Branch]
  :<|> "branches" :> Capture "name" Branch.Name :> GET View
  :<|> "branches" :> Capture "name" Branch.Name
       :> "submit" :> POST (Either PullRequestError PullRequest)
  :<|> "branches" :> Capture "name" Branch.Name
       :> "spaces" :> Body CreateSpaceBody :> POST Space
  :<|> "branches" :> Capture "name" Branch.Name
       :> "spaces" :> Capture "id" SpaceId :> Body UpdateSpaceBody :> PATCH Space
  :<|> "branches" :> Capture "name" Branch.Name
       :> "spaces" :> Capture "spaceId" SpaceId
       :> "properties" :> Capture "propertyId" PropertyId :> Body CreateTraitBody :> POST Trait
  :<|> "branches" :> Capture "name" Branch.Name
       :> "spaces" :> Capture "spaceId" SpaceId
       :> "properties" :> Capture "propertyId" PropertyId :> Body UpdateTraitBody :> PATCH Trait
  :<|> "branches" :> Capture "name" Branch.Name
       :> "properties" :> Body CreatePropertyBody :> POST Property
  :<|> "branches" :> Capture "name" Branch.Name
       :> "properties" :> Capture "id" PropertyId :> Body UpdatePropertyBody :> PATCH Property
  :<|> "branches" :> Capture "name" Branch.Name
       :> "theorems" :> Body CreateTheoremBody :> POST Theorem
  :<|> "branches" :> Capture "name" Branch.Name
       :> "theorems" :> Capture "id" TheoremId :> Body UpdateTheoremBody :> PATCH Theorem
  :<|> "me"     :> GET User
  :<|> "status" :> GET Status
  )

type instance AuthServerData (AuthProtect "token-auth") = Maybe User

{-
EXPLORE:

It would be nice not to have to choose a concrete effect stack here
(especially since doing so seems to require layering in effects like DB which
may be unused in some interpretations). This is difficult to get away from
currently because we're passing a natural transformation into `authHandler`
and so if we parametrize by Effects, we would need to construct a runnable

  nt :: forall r a. Members Effects r => Sem r a -> Handler a

but in general `r` could have arbitrary other unrelated effects.
-}
type Server = Sem
  '[ Properties
   , Spaces
   , Theorems
   , Traits
   , Branches
   , Repo
   , DB
   , Github
   , Http
   , Auth
   , Reader Status
   , Error Auth.LoginRequired
   , Error Auth.Forbidden
   , Error NotFound
   , Error ServerError
   , Embed IO
   ]

-- TODO: It seems like there should be a way to handle the global user setting
--   and almost-global branch setting without all this boilerplate. May need to
--   look into the natural transformations in Server
server :: ServerT API Server
server u = as u root
      :<|> as u branches
      :<|> (\b -> checkout u b $ viewBranch)
      :<|> (\b -> checkout u b $ submitBranch)
      :<|> (\b     body -> checkout u b $ createSpace       body)
      :<|> (\b  id body -> checkout u b $ updateSpace    id body)
      :<|> (\b s p body -> checkout u b $ createTrait   s p body)
      :<|> (\b s p body -> checkout u b $ updateTrait   s p body)
      :<|> (\b     body -> checkout u b $ createProperty    body)
      :<|> (\b  id body -> checkout u b $ updateProperty id body)
      :<|> (\b     body -> checkout u b $ createTheorem     body)
      :<|> (\b  id body -> checkout u b $ updateTheorem  id body)
      :<|> as u me
      :<|> as u status

as :: Member Auth r => Maybe User -> Sem r a -> Sem r a
as (Just u) a = Auth.login u >> a
as _ a = a

checkout :: Members '[Auth, Branches] r => Maybe User -> Branch.Name -> Sem r a -> Sem r a
checkout u b a = as u $ Branches.checkout (Branch b) >> a

context :: (forall a. Server a -> Handler a) -> Context '[AuthHandler Request (Maybe User)]
context nt = authHandler nt :. EmptyContext

authHandler :: (forall a. Server a -> Handler a) -> AuthHandler Request (Maybe User)
authHandler nt = mkAuthHandler $ \req ->
  case lookupHeader "Authorization" req of -- TODO: strip bearer
    Nothing    -> return Nothing
    Just token -> nt $ Auth.forToken token

lookupHeader :: HeaderName -> Request -> Maybe Text
lookupHeader name request = fmap decodeUtf8 $ Map.lookup name $ Map.fromList $ requestHeaders request

root :: Members '[Branches, Properties, Spaces, Theorems, Traits] r
     => Sem r View
root = do
  -- master <- Branches.master
  -- TODO: memoize by Branches.head master
  view

status :: Member (Reader Status) r => Sem r Status
status = ask

branches :: Members '[Auth, Branches] r
         => Sem r [Branch]
branches = Auth.currentUser >>= \case
  Just _ ->
    -- TODO: filter by user access
    Branches.all
  _ -> do
    master <- Branches.master
    return [master]

viewBranch :: Members '[ Auth
                       , Branches
                       , Error Auth.Forbidden
                       , Error Auth.LoginRequired
                       , Properties
                       , Spaces
                       , Theorems
                       , Traits
                       ] r
           => Sem r View
viewBranch = require Read $ view

submitBranch :: Members '[ Auth
                         , Branches
                         , Error Auth.Forbidden
                         , Error Auth.LoginRequired
                         ] r
             => Sem r (Either PullRequestError PullRequest)
submitBranch = require Admin $
  Branches.submit =<< Branches.current

createSpace :: Members '[ Auth
                        , Branches -- TODO: do these really need Branches, or just Reader Branch?
                        , Error Auth.Forbidden
                        , Error Auth.LoginRequired
                        , Spaces
                        ] r
            => CreateSpaceBody
            -> Sem r Space
createSpace = create spaceName $ \id CreateSpaceBody{..} ->
  Space
    { id
    , name
    , aliases
    , description
    , topology
    , refs
    }

updateSpace :: Members '[ Auth
                        , Branches
                        , Error Auth.Forbidden
                        , Error Auth.LoginRequired
                        , Error NotFound
                        , Spaces
                        ] r
            => SpaceId
            -> UpdateSpaceBody
            -> Sem r Space
updateSpace = update spaceName \UpdateSpaceBody{..} ->
    ( Space.nameL        .~ name )
  . ( Space.aliasesL     .~ aliases )
  . ( Space.descriptionL .~ description )
  . ( Space.topologyL    .~ topology )
  . ( Space.refsL        .~ refs )

createProperty :: Members '[ Auth
                           , Branches
                           , Error Auth.Forbidden
                           , Error Auth.LoginRequired
                           , Properties
                           ] r
               => CreatePropertyBody
               -> Sem r Property
createProperty = create propertyName $ \id CreatePropertyBody{..} ->
  Property
    { id
    , name
    , aliases
    , description
    , refs
    }

updateProperty :: Members '[ Auth
                           , Branches
                           , Error Auth.Forbidden
                           , Error Auth.LoginRequired
                           , Error NotFound
                           , Properties
                           ] r
               => PropertyId
               -> UpdatePropertyBody
               -> Sem r Property
updateProperty = update propertyName \UpdatePropertyBody{..} ->
    ( Property.nameL        .~ name )
  . ( Property.aliasesL     .~ aliases )
  . ( Property.descriptionL .~ description )
  . ( Property.refsL        .~ refs )

createTheorem :: Members '[ Auth
                          , Branches
                          , Error Auth.Forbidden
                          , Error Auth.LoginRequired
                          , Error NotFound
                          , Properties
                          , Theorems
                          ] r
              => CreateTheoremBody
              -> Sem r Theorem
createTheorem = create theoremName $ \id CreateTheoremBody{..} ->
  Theorem
    { id
    , implication
    , converse
    , description
    , refs
    }

updateTheorem :: Members '[ Auth
                          , Branches
                          , Error Auth.Forbidden
                          , Error Auth.LoginRequired
                          , Error NotFound
                          , Properties
                          , Theorems
                          ] r
              => TheoremId
              -> UpdateTheoremBody
              -> Sem r Theorem
updateTheorem = update theoremName \UpdateTheoremBody{..} ->
    ( Theorem.converseL    .~ converse )
  . ( Theorem.descriptionL .~ description )
  . ( Theorem.refsL        .~ refs )

createTrait :: Members '[ Auth
                        , Branches
                        , Error Auth.Forbidden
                        , Error Auth.LoginRequired
                        , Error NotFound
                        , Properties
                        , Spaces
                        , Traits
                        ] r
            => SpaceId
            -> PropertyId
            -> CreateTraitBody
            -> Sem r Trait
createTrait space property CreateTraitBody{..} = require Write $ do
  let trait = Trait
        { space
        , property
        , value
        , description
        , refs
        }
  name <- traitName trait
  Store.put @Trait (Added name) (space, property) trait
  return trait

updateTrait :: Members '[ Auth
                        , Branches
                        , Error Auth.Forbidden
                        , Error Auth.LoginRequired
                        , Error NotFound
                        , Properties
                        , Spaces
                        , Traits
                        ] r
            => SpaceId
            -> PropertyId
            -> UpdateTraitBody
            -> Sem r Trait
updateTrait = curry $ update traitName \UpdateTraitBody{..} ->
    ( Trait.descriptionL .~ description )
  . ( Trait.refsL        .~ refs )

me :: Members '[Auth, Error NotFound] r
   => Sem r User
me = fetch =<< Auth.currentUser

--

fetch :: Member (Error NotFound) r => Maybe a -> Sem r a
fetch (Just a) = return a
fetch _ = throw NotFound

create :: Members '[ Auth
                   , Branches
                   , Error Auth.Forbidden
                   , Error Auth.LoginRequired
                   , Store value key
                   ] r
       => (value -> Sem r Text)
       -> (key -> body -> value)
       -> body
       -> Sem r value
create name build body = require Write $ do
  id <- Store.next
  let value = build id body
  label <- name value
  Store.put (Added label) id value
  return value

update :: Members '[ Auth
                   , Branches
                   , Error Auth.Forbidden
                   , Error Auth.LoginRequired
                   , Error NotFound
                   , Store value key
                   ] r
       => (value -> Sem r Text)
       -> (body -> value -> value)
       -> key
       -> body
       -> Sem r value
update name build key body = require Write $ do
  current <- fetch =<< Store.get key
  let value = build body current
  label <- name value
  Store.put (Updated label) key value
  return value

require :: Members '[Auth, Branches, Error Auth.Forbidden, Error Auth.LoginRequired] r
        => Access
        -> Sem r a
        -> Sem r a
require required action = do
  branch <- Branches.current
  Auth.requireAccess branch required
  action

propertyName :: Monad m => Property -> m Text
propertyName = return . Property.name

spaceName :: Monad m => Space -> m Text
spaceName = return . Space.name

theoremName :: Members '[Error NotFound, Properties] r
            => Theorem -> Sem r Text
theoremName = fmap Implication.name
  . sequence
  . map (Properties.get >=> fetch >=> propertyName)
  . Theorem.implication

traitName :: Members '[Error NotFound, Spaces, Properties] r
          => Trait -> Sem r Text
traitName trait = do
  s <- fetch =<< Spaces.get     (Trait.space trait)
  p <- fetch =<< Properties.get (Trait.property trait)
  let
    label = case Trait.value trait of
      Trait.Value True  -> ""
      Trait.Value False -> "¬"
  return $ Space.name s <> ": " <> label <> Property.name p

view :: Members '[Branches, Properties, Spaces, Theorems, Traits] r
     => Sem r View
view = View
  <$> Store.all
  <*> Store.all
  <*> Store.all
  <*> Store.all
  <*> Branches.head
