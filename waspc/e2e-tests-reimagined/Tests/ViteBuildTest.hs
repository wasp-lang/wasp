module Tests.ViteBuildTest (viteBuildTests) where

import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import E2e
import NeatInterpolation (trimming)
import qualified StrongPath as SP
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory)
import System.FilePath ((</>))
import Wasp.Cli.Command.CreateNewProject.AvailableTemplates (minimalStarterTemplate)
import Wasp.Generator.WebAppGenerator (viteBuildDirPath)
import Wasp.Project.Env (dotEnvClient)

viteBuildTests :: [E2eTest]
viteBuildTests =
  testGroup
    "vite-build"
    [ test "fail-on-missing-required-env-vars"
        $ withBuiltProject
        $ runExpectingFailure viteBuild,
      test "success-with-required-env-vars"
        $ withBuiltProject
        $ run (withEnv [apiUrlEnvVar] viteBuild),
      test "fail-missing-inline-env-var" $
        withBuiltProject $ do
          run (withEnv [apiUrlEnvVar] viteBuild)
          assertBuildOutputDoesNotContain inlineEnvVarValue,
      -- Based on https://github.com/wasp-lang/wasp/issues/3741
      test "succeed-inline-env-var" $
        withBuiltProject $ do
          run (withEnv [apiUrlEnvVar, (testEnvVarKey, inlineEnvVarValue)] viteBuild)
          assertBuildOutputContains inlineEnvVarValue,
      test "ignore-dotenv-client-file-in-build" $
        withBuiltProject $ do
          writeDotEnvClientFile dotEnvFileValue
          run (withEnv [apiUrlEnvVar] viteBuild)
          assertBuildOutputDoesNotContain dotEnvFileValue,
      test "inline-env-vars-work-with-env-file-present" $
        withBuiltProject $ do
          writeDotEnvClientFile dotEnvFileValue
          run (withEnv [apiUrlEnvVar, (testEnvVarKey, inlineEnvVarValue)] viteBuild)
          assertBuildOutputContains inlineEnvVarValue,
      test "fail-on-user-code-type-error" $
        withBuiltProject $ do
          appendToFile "src/MainPage.tsx" typeError
          runExpectingFailure (withEnv [apiUrlEnvVar] viteBuild),
      test "ignore-wasp-ts-type-errors" $
        withBuiltProject $ do
          appendToFile "main.wasp.ts" typeError
          run (withEnv [apiUrlEnvVar] viteBuild),
      test "fail-on-user-defined-client-port" $
        withBuiltProject $ do
          writeToFile
            "vite.config.ts"
            [trimming|
              import { wasp } from "wasp/client/vite";
              import { defineConfig } from "vite";

              export default defineConfig({
                plugins: [wasp()],
                server: {
                  port: 4000,
                },
              });
            |]
          runExpectingFailure (withEnv [apiUrlEnvVar] viteBuild)
    ]
  where
    withBuiltProject :: TestM a -> TestM a
    withBuiltProject steps =
      withNewWaspProject minimalStarterTemplate $ do
        setDbToPostgres
        writeToFile
          "src/MainPage.tsx"
          [trimming|
            export function MainPage() {
              return <h2>{import.meta.env.${testEnvVarKeyText}}</h2>
            }
          |]
        wasp ["build"]
        steps

    viteBuild = command "npx" ["vite", "build"]

    assertBuildOutputContains :: String -> TestM ()
    assertBuildOutputContains value = do
      found <- buildOutputContains value
      assertTrue ("the Vite build output contains " ++ show value) found

    assertBuildOutputDoesNotContain :: String -> TestM ()
    assertBuildOutputDoesNotContain value = do
      found <- buildOutputContains value
      assertTrue ("the Vite build output doesn't contain " ++ show value) (not found)

    buildOutputContains :: String -> TestM Bool
    buildOutputContains value = do
      buildDir <- resolvePath (SP.fromRelDir viteBuildDirPath)
      liftIO $ anyFileContains (TE.encodeUtf8 $ T.pack value) buildDir

    writeDotEnvClientFile :: String -> TestM ()
    writeDotEnvClientFile value = writeToFile (SP.fromRelFile dotEnvClient) (T.pack $ testEnvVarKey ++ "=" ++ value)

    typeError = "const shouldBeString: string = 123"

    apiUrlEnvVar = ("REACT_APP_API_URL", "http://localhost:3001")

    testEnvVarKey = "REACT_APP_NAME"
    testEnvVarKeyText = T.pack testEnvVarKey

    inlineEnvVarValue = "RandomNameTest"

    dotEnvFileValue = "DotEnvFileValue"

anyFileContains :: BS.ByteString -> FilePath -> IO Bool
anyFileContains needle path = do
  isDir <- doesDirectoryExist path
  if isDir
    then listDirectory path >>= anyM (anyFileContains needle . (path </>))
    else do
      isFile <- doesFileExist path
      if isFile then BS.isInfixOf needle <$> BS.readFile path else return False
  where
    anyM _ [] = return False
    anyM predicate (x : xs) = predicate x >>= \result -> if result then return True else anyM predicate xs
