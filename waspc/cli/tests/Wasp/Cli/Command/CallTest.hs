module Wasp.Cli.Command.CallTest where

import Test.Hspec
import Wasp.Cli.Command.Call (Call (..), parseCall)

spec_parseCall :: Spec
spec_parseCall = do
  describe "parseCall" $ do
    it "parses no arguments as Help" $ do
      parseCall [] `shouldBe` Help

    it "parses the help command and help flags as Help" $ do
      parseCall ["help"] `shouldBe` Help
      parseCall ["--help"] `shouldBe` Help
      parseCall ["-h"] `shouldBe` Help

    it "parses commands without arguments" $ do
      parseCall ["clean"] `shouldBe` Clean
      parseCall ["install"] `shouldBe` Install
      parseCall ["compile"] `shouldBe` Compile
      parseCall ["uninstall"] `shouldBe` Uninstall
      parseCall ["version"] `shouldBe` Version
      parseCall ["doctor"] `shouldBe` Doctor
      parseCall ["build"] `shouldBe` Build
      parseCall ["telemetry"] `shouldBe` Telemetry
      parseCall ["deps"] `shouldBe` Deps
      parseCall ["dockerfile"] `shouldBe` Dockerfile
      parseCall ["news"] `shouldBe` News
      parseCall ["studio"] `shouldBe` Studio
      parseCall ["completion"] `shouldBe` PrintBashCompletionInstruction
      parseCall ["completion:list"] `shouldBe` BashCompletionListCommands

    it "passes arguments through to commands" $ do
      parseCall ["new"] `shouldBe` New []
      parseCall ["new", "MyApp"] `shouldBe` New ["MyApp"]
      parseCall ["start", "--client-port", "4000"]
        `shouldBe` Start ["--client-port", "4000"]
      parseCall ["start", "db"] `shouldBe` StartDb []
      parseCall ["start", "db", "--db-image", "postgres:13"]
        `shouldBe` StartDb ["--db-image", "postgres:13"]
      parseCall ["build", "start"] `shouldBe` BuildStart []
      parseCall ["db", "migrate-dev"] `shouldBe` Db ["migrate-dev"]
      parseCall ["deploy", "fly", "--org", "personal"]
        `shouldBe` Deploy ["fly", "--org", "personal"]
      parseCall ["test", "client"] `shouldBe` Test ["client"]
      parseCall ["show", "spec"] `shouldBe` Show ["spec"]

    it "parses unrecognized commands as Unknown" $ do
      parseCall ["bogus"] `shouldBe` Unknown ["bogus"]
      parseCall ["version", "extra"] `shouldBe` Unknown ["version", "extra"]
      parseCall ["--version"] `shouldBe` Unknown ["--version"]
