module Wasp.Cli.Util.EnvVarInputsTest where

import Control.Monad ((<=<))
import Data.List (isInfixOf)
import StrongPath (Abs, Dir, File', Path', Rel, relfile)
import qualified StrongPath as SP
import System.Environment (setEnv)
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec
import Wasp.Cli.Util.EnvVarInputs
  ( EnvVarInput (FromFlag, FromProjectFile, Inherit),
    describeEnvVarSources,
    mergeEnvVars,
    readEnvVarInput,
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

spec_mergeEnvVars :: Spec
spec_mergeEnvVars = do
  describe "mergeEnvVars" $ do
    it "returns no vars when there are no sources" $ do
      mergeEnvVars [] `shouldBe` []

    it "keeps the vars of every source, in the order the sources were given" $ do
      mergeEnvVars [(".env.server", [("FOO", "bar")]), ("--server-env", [("BAZ", "qux")])]
        `shouldBe` [("FOO", "bar"), ("BAZ", "qux")]

    it "lets the earlier source win when two sources set the same var" $ do
      mergeEnvVars
        [("your environment", [("FOO", "from the shell")]), (".env.server", [("FOO", "from the file")])]
        `shouldBe` [("FOO", "from the shell")]

spec_describeEnvVarSources :: Spec
spec_describeEnvVarSources = do
  describe "describeEnvVarSources" $ do
    it "says nothing when asked about no vars" $ do
      describeEnvVarSources [(".env.server", [("PORT", "8080")])] [] `shouldBe` ""

    it "names the source that sets the given var" $ do
      describeEnvVarSources [(".env.server", [("PORT", "8080"), ("FOO", "bar")])] ["PORT"]
        `shouldSatisfy` mentionsAll ["PORT", ".env.server"]

    it "leaves out the vars it wasn't asked about" $ do
      describeEnvVarSources [(".env.server", [("PORT", "8080"), ("FOO", "bar")])] ["PORT"]
        `shouldSatisfy` mentionsNone ["FOO"]

    it "names every source that sets the given var" $ do
      describeEnvVarSources
        [("your environment", [("PORT", "8080")]), (".env.server", [("PORT", "9090")])]
        ["PORT"]
        `shouldSatisfy` mentionsAll ["PORT", "your environment", ".env.server"]

withProjectDir :: (Path' Abs (Dir WaspProjectDir) -> IO a) -> IO a
withProjectDir action =
  withSystemTempDirectory "wasp-env-var-inputs-test" $ action <=< SP.parseAbsDir

serverDotEnvFile :: Path' (Rel WaspProjectDir) File'
serverDotEnvFile = [relfile|.env.server|]

mentionsAll :: [String] -> String -> Bool
mentionsAll expectedParts message = all (`isInfixOf` message) expectedParts

mentionsNone :: [String] -> String -> Bool
mentionsNone unexpectedParts message = not $ any (`isInfixOf` message) unexpectedParts
