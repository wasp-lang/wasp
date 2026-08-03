module Wasp.Cli.Util.EnvVarInputsTest where

import Control.Monad ((<=<))
import Data.Either (isLeft)
import Data.List (isInfixOf)
import StrongPath (Abs, Dir, File', Path', Rel, relfile)
import qualified StrongPath as SP
import System.Environment (setEnv)
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec
import Wasp.Cli.Util.EnvVarInputs
  ( EnvVarInput (FromFlag, FromProjectFile, Inherit),
    assertNoOverriddenEnvVars,
    readEnvVarInput,
    resolveEnvVars,
  )
import Wasp.Project.Common (WaspProjectDir)

spec_readEnvVarInput :: Spec
spec_readEnvVarInput = do
  describe "readEnvVarInput" $ do
    it "labels a var coming from a CLI option with that option's flag" $ do
      withProjectDir $ \projectDir ->
        readEnvVarInput projectDir (FromFlag "--server-env" ("FOO", "bar"))
          `shouldReturn` ("--server-env", [("FOO", "bar")])

    it "reads a project's dotenv file, labeling its vars with the file's path" $ do
      withProjectDir $ \projectDir -> do
        writeFile (SP.fromAbsFile $ projectDir SP.</> serverDotEnvFile) "FOO=bar\nBAZ=qux\n"
        readEnvVarInput projectDir (FromProjectFile serverDotEnvFile)
          `shouldReturn` (".env.server", [("FOO", "bar"), ("BAZ", "qux")])

    it "treats a missing project dotenv file as one that sets no vars" $ do
      withProjectDir $ \projectDir ->
        readEnvVarInput projectDir (FromProjectFile serverDotEnvFile)
          `shouldReturn` (".env.server", [])

    it "picks up the vars set in Wasp's own environment" $ do
      withProjectDir $ \projectDir -> do
        setEnv "WASP_ENV_VAR_INPUTS_TEST" "set in the shell"
        (source, envVars) <- readEnvVarInput projectDir Inherit
        source `shouldBe` "your environment"
        envVars `shouldContain` [("WASP_ENV_VAR_INPUTS_TEST", "set in the shell")]

spec_resolveEnvVars :: Spec
spec_resolveEnvVars = do
  describe "resolveEnvVars" $ do
    it "returns no vars when there is nothing to resolve" $ do
      resolveEnvVars [] [] `shouldBe` Right []

    it "keeps the wasp-owned vars and appends the ones the user set" $ do
      resolveEnvVars
        [("PORT", "3001")]
        [(".env.server", [("FOO", "bar")]), ("--server-env", [("BAZ", "qux")])]
        `shouldBe` Right [("PORT", "3001"), ("FOO", "bar"), ("BAZ", "qux")]

    it "lets the earlier source win when two sources set the same var" $ do
      resolveEnvVars
        []
        [("your environment", [("FOO", "from the shell")]), (".env.server", [("FOO", "from the file")])]
        `shouldBe` Right [("FOO", "from the shell")]

    it "fails when a source sets a wasp-owned var" $ do
      resolveEnvVars [("PORT", "3001")] [(".env.server", [("PORT", "8080")])]
        `shouldSatisfy` isLeft

spec_assertNoOverriddenEnvVars :: Spec
spec_assertNoOverriddenEnvVars = do
  describe "assertNoOverriddenEnvVars" $ do
    it "passes when no source sets a wasp-owned var" $ do
      assertNoOverriddenEnvVars
        [("PORT", "3001")]
        [(".env.server", [("FOO", "bar")]), ("your environment", [])]
        `shouldBe` Nothing

    it "reports the wasp-owned var and the source that sets it" $ do
      let errorMessage =
            assertNoOverriddenEnvVars
              [("PORT", "3001"), ("WASP_SERVER_URL", "http://localhost:3001")]
              [(".env.server", [("PORT", "8080"), ("FOO", "bar")])]
      errorMessage `shouldSatisfy` mentionsAll ["PORT", ".env.server"]
      errorMessage `shouldSatisfy` mentionsNone ["WASP_SERVER_URL", "FOO"]

    it "reports every source that sets a wasp-owned var" $ do
      let errorMessage =
            assertNoOverriddenEnvVars
              [("PORT", "3001")]
              [("your environment", [("PORT", "8080")]), (".env.server", [("PORT", "9090")])]
      errorMessage `shouldSatisfy` mentionsAll ["PORT", "your environment", ".env.server"]

withProjectDir :: (Path' Abs (Dir WaspProjectDir) -> IO a) -> IO a
withProjectDir action =
  withSystemTempDirectory "wasp-env-var-inputs-test" $ action <=< SP.parseAbsDir

serverDotEnvFile :: Path' (Rel WaspProjectDir) File'
serverDotEnvFile = [relfile|.env.server|]

mentionsAll :: [String] -> Maybe String -> Bool
mentionsAll expectedParts = maybe False (\message -> all (`isInfixOf` message) expectedParts)

mentionsNone :: [String] -> Maybe String -> Bool
mentionsNone unexpectedParts = maybe False (\message -> not $ any (`isInfixOf` message) unexpectedParts)
