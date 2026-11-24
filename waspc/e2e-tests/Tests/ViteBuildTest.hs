module Tests.ViteBuildTest (viteBuildTest) where

import Control.Monad (filterM)
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.Text as T
import E2E
import NeatInterpolation (trimming)
import qualified StrongPath as SP
import System.Directory (doesFileExist)
import System.Directory.Recursive (getDirRecursive)
import Tests.Fixtures (userCodeTypeError, userCodeTypeErrorMessage)
import Wasp.Cli.Command.CreateNewProject.AvailableTemplates (minimalStarterTemplate)
import Wasp.Generator.WebAppGenerator (viteBuildDirPath)
import Wasp.Generator.WebAppGenerator.Common (serverUrlEnvVarName)
import Wasp.Project.Env (dotEnvClient)

viteBuildTest :: E2ETest
viteBuildTest =
  testGroup
    "vite-build"
    [ viteBuildTestCase "fail-on-missing-required-env-vars" $
        runExpectingFailure (viteBuildCommand [])
          >>= assertOutputContains (T.pack serverUrlEnvVarName <> " is required"),
      viteBuildTestCase "succeed-with-required-env-vars" $
        run viteBuildWithApiUrlCommand,
      viteBuildTestCase "succeed-without-inline-env-var" $ do
        run viteBuildWithApiUrlCommand
        assertBuildDirDoesNotContain inlineEnvVarValue,
      -- Based on https://github.com/wasp-lang/wasp/issues/3741
      viteBuildTestCase "succeed-inline-env-var" $ do
        run $ withEnv [(testEnvVarKey, inlineEnvVarValue)] viteBuildWithApiUrlCommand
        assertBuildDirContains inlineEnvVarValue,
      viteBuildTestCase "ignore-dotenv-client-file-in-build" $ do
        writeDotEnvClientFile
        run viteBuildWithApiUrlCommand
        assertBuildDirDoesNotContain dotEnvFileValue,
      viteBuildTestCase "inline-env-vars-work-with-env-file-present" $ do
        writeDotEnvClientFile
        run $ withEnv [(testEnvVarKey, inlineEnvVarValue)] viteBuildWithApiUrlCommand
        assertBuildDirContains inlineEnvVarValue,
      viteBuildTestCase "fail-on-user-code-type-error" $ do
        appendToFile "src/MainPage.tsx" userCodeTypeError
        runExpectingFailure viteBuildWithApiUrlCommand >>= assertOutputContains userCodeTypeErrorMessage,
      viteBuildTestCase "ignore-wasp-ts-type-errors" $ do
        appendToFile "main.wasp.ts" userCodeTypeError
        run viteBuildWithApiUrlCommand,
      viteBuildTestCase "fail-on-user-defined-client-port" $ do
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
        runExpectingFailure viteBuildWithApiUrlCommand
          >>= assertOutputContains "\"server.port\" is set to 4000"
    ]
  where
    viteBuildTestCase :: String -> TestM a -> E2ETest
    viteBuildTestCase name steps =
      test name $
        withNewWaspProject minimalStarterTemplate $ do
          setDbToPostgres
          writeToFile
            "src/MainPage.tsx"
            [trimming|
              export function MainPage() {
                return <h2>${mainPageMarkerText}{import.meta.env.${testEnvVarKeyText}}</h2>
              }
            |]
          wasp ["build"]
          steps

    viteBuildWithApiUrlCommand :: Command
    viteBuildWithApiUrlCommand = withApiUrl $ viteBuildCommand []

    assertBuildDirContains :: String -> TestM ()
    assertBuildDirContains value =
      buildDirContains value >>= assertTrue ("the build dir contains " ++ show value)

    -- Also checks that the build dir has the main page, without which
    -- nothing would contain the value.
    assertBuildDirDoesNotContain :: String -> TestM ()
    assertBuildDirDoesNotContain value = do
      assertBuildDirContains mainPageMarker
      buildDirContains value >>= assertTrue ("the build dir doesn't contain " ++ show value) . not

    buildDirContains :: String -> TestM Bool
    buildDirContains value = do
      buildDir <- resolvePath $ SP.fromRelDir viteBuildDirPath
      liftIO $ do
        files <- getDirRecursive buildDir >>= filterM doesFileExist
        any (BSC.pack value `BS.isInfixOf`) <$> mapM BS.readFile files

    writeDotEnvClientFile :: TestM ()
    writeDotEnvClientFile = writeToFile (SP.fromRelFile dotEnvClient) $ T.pack $ testEnvVarKey ++ "=" ++ dotEnvFileValue

    mainPageMarker :: String
    mainPageMarker = "ViteBuildTestMainPage"

    mainPageMarkerText :: T.Text
    mainPageMarkerText = T.pack mainPageMarker

    testEnvVarKey :: String
    testEnvVarKey = "REACT_APP_NAME"

    testEnvVarKeyText :: T.Text
    testEnvVarKeyText = T.pack testEnvVarKey

    inlineEnvVarValue :: String
    inlineEnvVarValue = "RandomNameTest"

    dotEnvFileValue :: String
    dotEnvFileValue = "DotEnvFileValue"
