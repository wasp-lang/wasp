{-# LANGUAGE OverloadedRecordDot #-}

module Tests.ViteBuildTest (viteBuildTest) where

import Control.Monad.Reader (ask)
import qualified Data.Text as T
import NeatInterpolation (trimming)
import ShellCommands
  ( ShellCommand,
    ShellCommandBuilder,
    TestContext,
    WaspNewTemplate (..),
    WaspProjectContext (..),
    createFile,
    createTestWaspProject,
    inTestWaspProjectDir,
    setWaspDbToPSQL,
    waspCliBuild,
  )
import StrongPath (relfile, (</>))
import qualified StrongPath as SP
import Test (Test (..), TestCase (..))
import Wasp.Generator.WebAppGenerator (viteBuildDirPath)
import Wasp.Project.Env (dotEnvClient)

viteBuildTest :: Test
viteBuildTest =
  Test
    "vite-build-env-vars"
    [ TestCase
        "fail-on-missing-required-env-vars"
        ( createViteBuildTestCase [expectCommandFailure <$> viteBuild]
        ),
      TestCase
        "success-with-required-env-vars"
        ( createViteBuildTestCase [appendInlineEnvVars [apiUrlEnvVar] <$> viteBuild]
        ),
      TestCase
        "fail-missing-inline-env-var"
        ( createViteBuildTestCase
            [ appendInlineEnvVars [apiUrlEnvVar] <$> viteBuild,
              expectCommandFailure <$> assertBuildOutputContains inlineEnvVarValue
            ]
        ),
      TestCase
        -- Based on https://github.com/wasp-lang/wasp/issues/3741
        "succeed-inline-env-var"
        ( createViteBuildTestCase
            [ appendInlineEnvVars [apiUrlEnvVar, (testEnvVarKey, inlineEnvVarValue)] <$> viteBuild,
              assertBuildOutputContains inlineEnvVarValue
            ]
        ),
      TestCase
        "ignore-dotenv-client-file-in-build"
        ( createViteBuildTestCase
            [ writeDotEnvClientFile dotEnvFileValue,
              appendInlineEnvVars [apiUrlEnvVar] <$> viteBuild,
              expectCommandFailure <$> assertBuildOutputContains dotEnvFileValue
            ]
        ),
      TestCase
        "inline-env-vars-work-with-env-file-present"
        ( createViteBuildTestCase
            [ writeDotEnvClientFile dotEnvFileValue,
              appendInlineEnvVars [apiUrlEnvVar, (testEnvVarKey, inlineEnvVarValue)] <$> viteBuild,
              assertBuildOutputContains inlineEnvVarValue
            ]
        )
    ]
  where
    createViteBuildTestCase :: [ShellCommandBuilder WaspProjectContext ShellCommand] -> ShellCommandBuilder TestContext [ShellCommand]
    createViteBuildTestCase commands =
      sequence
        [ createTestWaspProject Minimal,
          inTestWaspProjectDir $ [setWaspDbToPSQL, writeMainPageTsx, waspCliBuild] ++ commands
        ]

    viteBuild :: ShellCommandBuilder WaspProjectContext ShellCommand
    viteBuild = return "npx vite build"

    assertBuildOutputContains :: String -> ShellCommandBuilder WaspProjectContext ShellCommand
    assertBuildOutputContains value = return $ "grep -r '" ++ value ++ "' " ++ SP.fromRelDir viteBuildDirPath

    writeMainPageTsx :: ShellCommandBuilder WaspProjectContext ShellCommand
    writeMainPageTsx = do
      waspProjectContext <- ask
      let testEnvVarKeyText = T.pack testEnvVarKey
      createFile
        (waspProjectContext.waspProjectDir </> [relfile|src/MainPage.tsx|])
        [trimming|
          export function MainPage() {
            return <h2>{import.meta.env.${testEnvVarKeyText}}</h2>
          }
        |]

    writeDotEnvClientFile :: String -> ShellCommandBuilder WaspProjectContext ShellCommand
    writeDotEnvClientFile value = do
      waspProjectContext <- ask
      createFile (waspProjectContext.waspProjectDir </> dotEnvClient) $
        T.pack $
          testEnvVarKey ++ "=" ++ value

    appendInlineEnvVars :: [(String, String)] -> ShellCommand -> ShellCommand
    appendInlineEnvVars envVars command = foldr appendInlineEnvVar command envVars

    appendInlineEnvVar :: (String, String) -> ShellCommand -> ShellCommand
    appendInlineEnvVar (key, value) command = key ++ "=" ++ value ++ " " ++ command

    apiUrlEnvVar :: (String, String)
    apiUrlEnvVar = ("REACT_APP_API_URL", "http://localhost:3001")

    testEnvVarKey :: String
    testEnvVarKey = "REACT_APP_NAME"

    inlineEnvVarValue :: String
    inlineEnvVarValue = "RandomNameTest"

    dotEnvFileValue :: String
    dotEnvFileValue = "DotEnvFileValue"

    expectCommandFailure :: ShellCommand -> ShellCommand
    expectCommandFailure command = "! " ++ command
