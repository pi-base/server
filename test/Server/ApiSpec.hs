module Server.ApiSpec (spec) where

import Test.Import
import Test.Fixtures

import Server.Import
import Server.Api    as Server

import qualified Data.Branch        as Branch
import qualified Data.Property      as Property
import qualified Data.Space         as Space
import qualified Data.Trait         as Trait
import           Interpreter        as I
import           Persist.Auth       (Forbidden(..))
import qualified Persist.Auth       as Auth
import qualified Persist.Branches   as Branches
import qualified Persist.Properties as Properties
import qualified Persist.Spaces     as Spaces
import qualified Persist.Theorems   as Theorems
import qualified Persist.Traits     as Traits
import           Polysemy.Error     (Error)
import           Polysemy.Reader    (Reader)
import qualified Server.View        as View

type Effects =
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
  , Error Auth.Forbidden
  , Error Auth.LoginRequired
  , Error NotFound
  , Error Ex
  , Expect
  , Fail
  , Embed IO
  ]

spec :: IO TestTree
spec = testSpec "Server.Api" $ parallel $ do
  env <- runIO $ I.boot $ I.pure test testStatus

  let
    eval' :: Sem Effects a -> IO (Either Ex a)
    eval' action = do
      I.run env action
      & I.gatherErrors
      & runTest

    eval :: Sem Effects a -> IO ()
    eval = void . eval'

  -- TODO: option for running full tests with production interpretation
  --       (except http), at least for CI

  describe "status" $ do
    it "returns the server status" $ eval $ do
      testStatus <== Server.status

  describe "branches" $ do
    let users = [james, steven, rachel, leah]

    it "always includes the master branch" $ eval $ do
      mapM_ createUserBranch users
      [test] <== branches

    -- FIXME
    -- it "shows admins all branches" $ eval $ do
    --   found <- mapM createUserBranch users
    --   as james
    --   [test] <> found <== branches

    -- it "shows standard users their branch" $ eval $ do
    --   mapM_ createUserBranch users
    --   as leah
    --   let branch = Branch.forUser leah
    --   [test, branch] <== branches

  describe "me" $ do
    it "returns the user when logged in" $ eval $ do
      Auth.login james

      james <== me

    it "raises not found otherwise" $ do
      -- TODO: more natural handling of asserting about thrown errors
      Left e <- eval' me
      e `shouldBe` NotFoundError NotFound

  describe "root" $ do
    it "shows data" $ eval $ do
      Spaces.put     setup finiteDiscrete
      Properties.put setup compact
      Properties.put setup countablyCompact
      Theorems.put   setup compactIsCountablyCompact
      Traits.put     setup finiteDiscreteIsCompact

      view <- root

      View.spaces     view === [finiteDiscrete]
      View.properties view === [compact, countablyCompact]
      View.theorems   view === [compactIsCountablyCompact]
      View.traits     view === [finiteDiscreteIsCompact]

  describe "createSpace" $ do
    let
      body = CreateSpaceBody
        { name        = "New Space"
        , aliases     = []
        , description = ""
        , topology    = Nothing
        , refs        = []
        }

    it "can add a space to a branch" $ eval $ do
      as james
      s <- createSpace body
      Space.name s === "New Space"

    it "does not allow non-admins to write to other user branches" $ do
      Left err <- eval' $ do
        Auth.login leah
        Branches.checkout $ Branch.forUser james
        createSpace body
      err `shouldBe` ForbiddenError Forbidden

    it "does allow admins to write to other user branches" $ eval $ do
      Auth.login james
      Branches.checkout $ Branch.forUser rachel
      s <- createSpace body
      Space.name s === "New Space"

    it "does not allow admins to write to master" $ do
      Left err <- eval' $ do
        Auth.login james
        Branches.checkout =<< Branches.master
        createSpace body
      err `shouldBe` ForbiddenError Forbidden

  describe "traitName" $ do
    let
      trait = Trait (Space.id finiteDiscrete) (Property.id compact) (Trait.Value False) "" []

    it "formats the name if both space and property exist" $ eval $ do
      Spaces.put setup finiteDiscrete
      Properties.put setup compact

      "Finite Discrete Topology: ¬Compact" <== traitName trait

    it "throws if the space is missing" $ do
      Left err <- eval' $ traitName trait
      err `shouldBe` NotFoundError NotFound

  describe "theoremName" $ do
    it "formats the name if all properties exist" $ eval $ do
      Properties.put setup compact
      Properties.put setup countablyCompact

      "Compact => Countably Compact" <== theoremName compactIsCountablyCompact

    it "throws if a property is missing" $ do
      Left err <- eval' $ do
        Properties.put setup compact
        theoremName compactIsCountablyCompact
      err `shouldBe` NotFoundError NotFound

as :: Members '[Auth, Branches] r
   => User -> Sem r ()
as user = do
  Auth.login user
  Branches.checkout =<< createUserBranch user

createUserBranch :: Members '[Auth, Branches] r
                 => User -> Sem r Branch
createUserBranch user = do
  let branch = Branch.forUser user
  Auth.grant user branch Auth.Write
  return branch
