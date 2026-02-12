{-# LANGUAGE OverloadedRecordDot #-}

module Tests.ViteBuildTest (viteBuildTest) where

import Control.Monad.Reader (ask)
import qualified Data.Text as T
import NeatInterpolation (trimming)
import ShellCommands
  ( ShellCommand,
    ShellCommandBuilder,
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

viteBuildTest :: Test
viteBuildTest =
  Test
    "loading-env-var-vite-build"
    [ TestCase
        "fail-missing-inline-env-var"
        ( sequence
            [ createTestWaspProject Minimal,
              inTestWaspProjectDir
                [ setWaspDbToPSQL,
                  writeMainPageTsx,
                  waspCliBuild,
                  viteBuild,
                  expectCommandFailure <$> assertBuildOutputContains testEnvVarValue
                ]
            ]
        ),
      TestCase
        -- Based on https://github.com/wasp-lang/wasp/issues/3741
        "succeed-inline-env-var"
        ( sequence
            [ createTestWaspProject Minimal,
              inTestWaspProjectDir
                [ setWaspDbToPSQL,
                  writeMainPageTsx,
                  waspCliBuild,
                  appendInlineEnvVar testEnvVarKey testEnvVarValue <$> viteBuild,
                  assertBuildOutputContains testEnvVarValue
                ]
            ]
        )
    ]
  where
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

    appendInlineEnvVar :: String -> String -> ShellCommand -> ShellCommand
    appendInlineEnvVar envVarName envVarValue command = envVarName ++ "=" ++ envVarValue ++ " " ++ command

    testEnvVarKey :: String
    testEnvVarKey = "REACT_APP_NAME"

    testEnvVarValue :: String
    testEnvVarValue = "RandomNameTest"

    expectCommandFailure :: ShellCommand -> ShellCommand
    expectCommandFailure command = "! " ++ command
