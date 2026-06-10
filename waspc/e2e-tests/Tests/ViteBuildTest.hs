module Tests.ViteBuildTest (viteBuildTest) where

import Command (Command, cmd, withEnvVars)
import Context (TestContext, WaspProjectContext (..))
import qualified Data.Text as T
import NeatInterpolation (trimming)
import Step (Step, askStepContext)
import Steps
  ( appendToFile,
    assertAnyFileInDirContainsText,
    assertNoFileInDirContainsText,
    createTestWaspProject,
    inTestWaspProjectDir,
    runCommand,
    runCommandExpectingFailure,
    setWaspDbToPSQL,
    waspCliBuild,
    writeToFile,
  )
import StrongPath (relfile, (</>))
import Test (Test (..), TestCase (..))
import Wasp.Cli.Command.CreateNewProject.AvailableTemplates (minimalStarterTemplate)
import Wasp.Generator.WebAppGenerator (viteBuildDirPath)
import Wasp.Project.Env (dotEnvClient)

viteBuildTest :: Test
viteBuildTest =
  Test
    "vite-build"
    [ TestCase
        "fail-on-missing-required-env-vars"
        (createViteBuildTestCase [runCommandExpectingFailure viteBuild]),
      TestCase
        "success-with-required-env-vars"
        (createViteBuildTestCase [runCommand $ withEnvVars [apiUrlEnvVar] viteBuild]),
      TestCase
        "fail-missing-inline-env-var"
        ( createViteBuildTestCase
            [ runCommand $ withEnvVars [apiUrlEnvVar] viteBuild,
              assertNoFileInDirContainsText viteBuildDirPath inlineEnvVarValue
            ]
        ),
      TestCase
        -- Based on https://github.com/wasp-lang/wasp/issues/3741
        "succeed-inline-env-var"
        ( createViteBuildTestCase
            [ runCommand $ withEnvVars [apiUrlEnvVar, (testEnvVarKey, inlineEnvVarValue)] viteBuild,
              assertAnyFileInDirContainsText viteBuildDirPath inlineEnvVarValue
            ]
        ),
      TestCase
        "ignore-dotenv-client-file-in-build"
        ( createViteBuildTestCase
            [ writeDotEnvClientFile dotEnvFileValue,
              runCommand $ withEnvVars [apiUrlEnvVar] viteBuild,
              assertNoFileInDirContainsText viteBuildDirPath dotEnvFileValue
            ]
        ),
      TestCase
        "inline-env-vars-work-with-env-file-present"
        ( createViteBuildTestCase
            [ writeDotEnvClientFile dotEnvFileValue,
              runCommand $ withEnvVars [apiUrlEnvVar, (testEnvVarKey, inlineEnvVarValue)] viteBuild,
              assertAnyFileInDirContainsText viteBuildDirPath inlineEnvVarValue
            ]
        ),
      TestCase
        "fail-on-user-code-type-error"
        ( createViteBuildTestCase
            [ addTypeErrorToSrcFile,
              runCommandExpectingFailure viteBuildWithApiUrl
            ]
        ),
      TestCase
        "ignore-wasp-ts-type-errors"
        ( createViteBuildTestCase
            [ addTypeErrorToWaspTsFile,
              runCommand viteBuildWithApiUrl
            ]
        )
    ]
  where
    createViteBuildTestCase :: [Step WaspProjectContext ()] -> [Step TestContext ()]
    createViteBuildTestCase steps =
      [ createTestWaspProject minimalStarterTemplate,
        inTestWaspProjectDir $ [setWaspDbToPSQL, writeMainPageTsx, runCommand waspCliBuild] ++ steps
      ]

    viteBuild :: Command
    viteBuild = cmd "npx" ["vite", "build"]

    viteBuildWithApiUrl :: Command
    viteBuildWithApiUrl = withEnvVars [apiUrlEnvVar] viteBuild

    writeMainPageTsx :: Step WaspProjectContext ()
    writeMainPageTsx = do
      waspProjectContext <- askStepContext
      let testEnvVarKeyText = T.pack testEnvVarKey
      writeToFile
        (waspProjectContext.waspProjectDir </> [relfile|src/MainPage.tsx|])
        [trimming|
          export function MainPage() {
            return <h2>{import.meta.env.${testEnvVarKeyText}}</h2>
          }
        |]

    writeDotEnvClientFile :: String -> Step WaspProjectContext ()
    writeDotEnvClientFile value = do
      waspProjectContext <- askStepContext
      writeToFile (waspProjectContext.waspProjectDir </> dotEnvClient) $
        T.pack $
          testEnvVarKey ++ "=" ++ value

    addTypeErrorToSrcFile :: Step WaspProjectContext ()
    addTypeErrorToSrcFile = appendToFile "src/MainPage.tsx" typeError

    addTypeErrorToWaspTsFile :: Step WaspProjectContext ()
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
