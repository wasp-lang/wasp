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
import Wasp.Cli.Command.CreateNewProject.AvailableTemplates (minimalStarterTemplate)
import Wasp.Generator.WebAppGenerator (viteBuildDirPath)
import Wasp.Project.Env (dotEnvClient)

viteBuildTest :: E2ETest
viteBuildTest =
  testGroup
    "vite-build"
    [ viteBuildTestCase "fail-on-missing-required-env-vars" $
        runExpectingFailure viteBuild,
      viteBuildTestCase "success-with-required-env-vars" $
        run viteBuildWithApiUrl,
      viteBuildTestCase "fail-missing-inline-env-var" $ do
        run viteBuildWithApiUrl
        assertBuildOutputDoesNotContain inlineEnvVarValue,
      -- Based on https://github.com/wasp-lang/wasp/issues/3741
      viteBuildTestCase "succeed-inline-env-var" $ do
        run $ withEnv [(testEnvVarKey, inlineEnvVarValue)] viteBuildWithApiUrl
        assertBuildOutputContains inlineEnvVarValue,
      viteBuildTestCase "ignore-dotenv-client-file-in-build" $ do
        writeDotEnvClientFile
        run viteBuildWithApiUrl
        assertBuildOutputDoesNotContain dotEnvFileValue,
      viteBuildTestCase "inline-env-vars-work-with-env-file-present" $ do
        writeDotEnvClientFile
        run $ withEnv [(testEnvVarKey, inlineEnvVarValue)] viteBuildWithApiUrl
        assertBuildOutputContains inlineEnvVarValue,
      viteBuildTestCase "fail-on-user-code-type-error" $ do
        appendToFile "src/MainPage.tsx" typeError
        runExpectingFailure viteBuildWithApiUrl,
      viteBuildTestCase "ignore-wasp-ts-type-errors" $ do
        appendToFile "main.wasp.ts" typeError
        run viteBuildWithApiUrl,
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
        runExpectingFailure viteBuildWithApiUrl
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
                return <h2>{import.meta.env.${testEnvVarKeyText}}</h2>
              }
            |]
          wasp ["build"]
          steps

    viteBuild :: Command
    viteBuild = nodeModulesBin "vite" ["build"]

    viteBuildWithApiUrl :: Command
    viteBuildWithApiUrl = withEnv [("REACT_APP_API_URL", "http://localhost:3001")] viteBuild

    assertBuildOutputContains :: String -> TestM ()
    assertBuildOutputContains value =
      buildOutputContains value >>= assertTrue ("the build output contains " ++ show value)

    assertBuildOutputDoesNotContain :: String -> TestM ()
    assertBuildOutputDoesNotContain value =
      buildOutputContains value >>= assertTrue ("the build output doesn't contain " ++ show value) . not

    buildOutputContains :: String -> TestM Bool
    buildOutputContains value = do
      buildDir <- resolvePath $ SP.fromRelDir viteBuildDirPath
      liftIO $ do
        files <- getDirRecursive buildDir >>= filterM doesFileExist
        any (BSC.pack value `BS.isInfixOf`) <$> mapM BS.readFile files

    writeDotEnvClientFile :: TestM ()
    writeDotEnvClientFile = writeToFile (SP.fromRelFile dotEnvClient) $ T.pack $ testEnvVarKey ++ "=" ++ dotEnvFileValue

    typeError :: T.Text
    typeError = "const shouldBeString: string = 123"

    testEnvVarKey :: String
    testEnvVarKey = "REACT_APP_NAME"

    testEnvVarKeyText :: T.Text
    testEnvVarKeyText = T.pack testEnvVarKey

    inlineEnvVarValue :: String
    inlineEnvVarValue = "RandomNameTest"

    dotEnvFileValue :: String
    dotEnvFileValue = "DotEnvFileValue"
