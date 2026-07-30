module Tests.ViteBuildTest (viteBuildTest) where

import Command (Command, cmd, withEnvVars)
import Context (TestContext, WaspProjectContext (..))
import Control.Monad.Reader (ask)
import qualified Data.Text as T
import NeatInterpolation (trimming)
import SharedActions
  ( appendToFile,
    createWaspProject,
    inWaspProjectDir,
    runCommand,
    runCommandExpectingFailure,
    setWaspDbToPSQL,
    waspCliBuild,
    writeToFile,
  )
import StrongPath (relfile, (</>))
import qualified StrongPath as SP
import Test (Test (..), TestCase (..))
import TestAction (TestAction)
import Wasp.Cli.Command.CreateNewProject.AvailableTemplates (minimalStarterTemplate)
import Wasp.Generator.WebAppGenerator (viteBuildDirPath)
import Wasp.Project.Env (dotEnvClient)

viteBuildTest :: Test
viteBuildTest =
  Test
    "vite-build"
    [ TestCase "fail-on-missing-required-env-vars" $
        createViteBuildTestCase $
          runCommandExpectingFailure viteBuild,
      TestCase "success-with-required-env-vars" $
        createViteBuildTestCase $
          runCommand $
            withEnvVars [apiUrlEnvVar] viteBuild,
      TestCase "fail-missing-inline-env-var" $
        createViteBuildTestCase $ do
          runCommand $ withEnvVars [apiUrlEnvVar] viteBuild
          runCommandExpectingFailure $ assertBuildOutputContains inlineEnvVarValue,
      -- Based on https://github.com/wasp-lang/wasp/issues/3741
      TestCase "succeed-inline-env-var" $
        createViteBuildTestCase $ do
          runCommand $ withEnvVars [apiUrlEnvVar, (testEnvVarKey, inlineEnvVarValue)] viteBuild
          runCommand $ assertBuildOutputContains inlineEnvVarValue,
      TestCase "ignore-dotenv-client-file-in-build" $
        createViteBuildTestCase $ do
          writeDotEnvClientFile dotEnvFileValue
          runCommand $ withEnvVars [apiUrlEnvVar] viteBuild
          runCommandExpectingFailure $ assertBuildOutputContains dotEnvFileValue,
      TestCase "inline-env-vars-work-with-env-file-present" $
        createViteBuildTestCase $ do
          writeDotEnvClientFile dotEnvFileValue
          runCommand $ withEnvVars [apiUrlEnvVar, (testEnvVarKey, inlineEnvVarValue)] viteBuild
          runCommand $ assertBuildOutputContains inlineEnvVarValue,
      TestCase "fail-on-user-code-type-error" $
        createViteBuildTestCase $ do
          addTypeErrorToSrcFile
          runCommandExpectingFailure viteBuildWithApiUrl,
      TestCase "ignore-wasp-ts-type-errors" $
        createViteBuildTestCase $ do
          addTypeErrorToWaspTsFile
          runCommand viteBuildWithApiUrl
    ]
  where
    createViteBuildTestCase :: TestAction WaspProjectContext () -> TestAction TestContext ()
    createViteBuildTestCase actions = do
      createWaspProject minimalStarterTemplate
      inWaspProjectDir $ do
        setWaspDbToPSQL
        writeMainPageTsx
        runCommand waspCliBuild
        actions

    viteBuild :: Command
    viteBuild = cmd "npx" ["vite", "build"]

    viteBuildWithApiUrl :: Command
    viteBuildWithApiUrl = withEnvVars [apiUrlEnvVar] viteBuild

    assertBuildOutputContains :: String -> Command
    assertBuildOutputContains value = cmd "grep" ["-r", value, SP.fromRelDir viteBuildDirPath]

    writeMainPageTsx :: TestAction WaspProjectContext ()
    writeMainPageTsx = do
      waspProjectContext <- ask
      let testEnvVarKeyText = T.pack testEnvVarKey
      writeToFile
        (waspProjectContext.waspProjectDir </> [relfile|src/MainPage.tsx|])
        [trimming|
          export function MainPage() {
            return <h2>{import.meta.env.${testEnvVarKeyText}}</h2>
          }
        |]

    writeDotEnvClientFile :: String -> TestAction WaspProjectContext ()
    writeDotEnvClientFile value = do
      waspProjectContext <- ask
      writeToFile (waspProjectContext.waspProjectDir </> dotEnvClient) $
        T.pack $
          testEnvVarKey ++ "=" ++ value

    addTypeErrorToSrcFile :: TestAction WaspProjectContext ()
    addTypeErrorToSrcFile = appendToFile "src/MainPage.tsx" typeError

    addTypeErrorToWaspTsFile :: TestAction WaspProjectContext ()
    addTypeErrorToWaspTsFile = appendToFile "main.wasp.ts" typeError

    typeError :: T.Text
    typeError = "const shouldBeString: string = 123"

    apiUrlEnvVar :: (String, String)
    apiUrlEnvVar = ("REACT_APP_API_URL", "http://localhost:3001")

    testEnvVarKey :: String
    testEnvVarKey = "REACT_APP_NAME"

    inlineEnvVarValue :: String
    inlineEnvVarValue = "RandomNameTest"

    dotEnvFileValue :: String
    dotEnvFileValue = "DotEnvFileValue"
