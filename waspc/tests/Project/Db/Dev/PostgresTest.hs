module Project.Db.Dev.PostgresTest where

import Data.List (isInfixOf)
import Fixtures (systemSPRoot)
import StrongPath (reldir, (</>))
import Test.Hspec
import Wasp.Project.Db.Dev.Postgres (makeDevConnectionUrl, makeDevPort)

spec_MakeDevPort :: Spec
spec_MakeDevPort = do
  describe "makeDevPort" $ do
    it "returns a port inside the [15432, 20431] range" $ do
      let port = makeDevPort (systemSPRoot </> [reldir|some/project|]) "MyApp"
      port `shouldSatisfy` (>= 15432)
      port `shouldSatisfy` (<= 20431)

    it "is deterministic: same inputs -> same port" $ do
      let projectDir = systemSPRoot </> [reldir|some/project|]
      makeDevPort projectDir "MyApp" `shouldBe` makeDevPort projectDir "MyApp"

    it "avoids the default PostgreSQL port 5432" $ do
      let port = makeDevPort (systemSPRoot </> [reldir|some/project|]) "MyApp"
      port `shouldNotBe` 5432

    it "produces different ports for different project paths (typical case)" $ do
      let portA = makeDevPort (systemSPRoot </> [reldir|projectA|]) "App"
      let portB = makeDevPort (systemSPRoot </> [reldir|projectB|]) "App"
      portA `shouldNotBe` portB

    it "produces different ports for different app names (typical case)" $ do
      let projectDir = systemSPRoot </> [reldir|some/project|]
      makeDevPort projectDir "AppA" `shouldNotBe` makeDevPort projectDir "AppB"

  describe "makeDevConnectionUrl" $ do
    it "embeds the derived dev port in the connection URL" $ do
      let projectDir = systemSPRoot </> [reldir|some/project|]
      let port = makeDevPort projectDir "MyApp"
      let url = makeDevConnectionUrl projectDir "MyApp"
      (":" <> show port <> "/") `isInfixOf` url `shouldBe` True
