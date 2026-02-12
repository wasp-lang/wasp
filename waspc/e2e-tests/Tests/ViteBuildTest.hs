{-# LANGUAGE OverloadedRecordDot #-}

module Tests.ViteBuildTest (viteBuildTest) where

import Control.Monad.Reader (ask)
import qualified Data.Text as T
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
import Test (Test (..), TestCase (..))

viteBuildTest :: Test
viteBuildTest =
  Test
    "vite-build"
    [ TestCase
        "fail-missing-inline-env-var"
        ( sequence
            [ createTestWaspProject Minimal,
              inTestWaspProjectDir
                [ setWaspDbToPSQL,
                  writeMainPageTsx,
                  waspCliBuild,
                  return viteBuild,
                  return $ expectCommandFailure $ assertBuildOutputContains testEnvVarValue
                ]
            ]
        ),
      TestCase
        "succeed-inline-env-var"
        ( sequence
            [ createTestWaspProject Minimal,
              inTestWaspProjectDir
                [ setWaspDbToPSQL,
                  writeMainPageTsx,
                  waspCliBuild,
                  return $ addInlineEnvVarToCommand viteBuild testEnvVarValue,
                  return $ assertBuildOutputContains testEnvVarValue
                ]
            ]
        )
    ]
  where
    testEnvVarValue :: String
    testEnvVarValue = "RandomNameTest"

    viteBuild :: ShellCommand
    viteBuild = "npx vite build"

    assertBuildOutputContains :: String -> ShellCommand
    assertBuildOutputContains value = "grep -r '" ++ value ++ "' .wasp/out/web-app/build/"

    addInlineEnvVarToCommand :: ShellCommand -> String -> ShellCommand
    addInlineEnvVarToCommand command value = "REACT_APP_NAME=" ++ value ++ " " ++ command

    expectCommandFailure :: ShellCommand -> ShellCommand
    expectCommandFailure command = "! " ++ command

    writeMainPageTsx :: ShellCommandBuilder WaspProjectContext ShellCommand
    writeMainPageTsx = do
      waspProjectContext <- ask
      createFile (waspProjectContext.waspProjectDir </> [relfile|src/MainPage.tsx|]) $
        T.unlines
          [ "export function MainPage() {",
            "  return <h2>{import.meta.env.REACT_APP_NAME}</h2>",
            "}"
          ]
