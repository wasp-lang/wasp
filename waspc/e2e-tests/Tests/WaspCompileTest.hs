module Tests.WaspCompileTest (waspCompileTest) where

import Control.Monad.Reader (ask)
import qualified Data.Text as T
import ShellCommands
  ( ShellCommand,
    ShellCommandBuilder,
    WaspProjectContext (..),
    appendToFile,
    assertCommandOutputContains,
    createTestWaspProject,
    inTestWaspProjectDir,
    waspCliCompile,
    writeToFile,
  )
import StrongPath (relfile, (</>))
import Test (Test (..), TestCase (..))
import Wasp.Cli.Command.CreateNewProject.AvailableTemplates (minimalStarterTemplate)

waspCompileTest :: Test
waspCompileTest =
  Test
    "wasp-compile"
    [ TestCase
        "fail-outside-project"
        (return [waspCliCompileFails]),
      TestCase
        "succeed-uncompiled-project"
        ( sequence
            [ createTestWaspProject minimalStarterTemplate,
              inTestWaspProjectDir
                [ waspCliCompile,
                  return $ assertDirectoryExists ".wasp",
                  return $ assertDirectoryExists "node_modules"
                ]
            ]
        ),
      TestCase
        "succeed-compiled-project"
        ( sequence
            [ createTestWaspProject minimalStarterTemplate,
              inTestWaspProjectDir
                [ waspCliCompile,
                  waspCliCompile,
                  return $ assertDirectoryExists ".wasp",
                  return $ assertDirectoryExists "node_modules"
                ]
            ]
        ),
      -- Regression test for https://github.com/wasp-lang/wasp/issues/2001
      TestCase
        "fail-missing-side-effect-import"
        ( sequence
            [ createTestWaspProject minimalStarterTemplate,
              inTestWaspProjectDir
                [ appendToFile "src/MainPage.tsx" "import './missing-side-effect-import'",
                  assertCommandOutputContains
                    (("! " ++) <$> waspCliCompile)
                    "Cannot find module or type declarations for side-effect import of './missing-side-effect-import'."
                ]
            ]
        ),
      TestCase
        "fail-type-error"
        ( sequence
            [ createTestWaspProject minimalStarterTemplate,
              inTestWaspProjectDir
                [ writeUtilsFile "export const shouldBeNumber: number = 'wrong'",
                  assertCommandOutputContains
                    (("! " ++) <$> waspCliCompile)
                    "src/utils.ts(1,14): error TS2322: Type 'string' is not assignable to type 'number'."
                ]
            ]
        )
    ]
  where
    waspCliCompileFails :: ShellCommand
    waspCliCompileFails = "! $WASP_CLI_CMD compile"

    assertDirectoryExists :: FilePath -> ShellCommand
    assertDirectoryExists dirFilePath = "[ -d '" ++ dirFilePath ++ "' ]"

    writeUtilsFile :: T.Text -> ShellCommandBuilder WaspProjectContext ShellCommand
    writeUtilsFile contents = do
      context <- ask
      writeToFile (context.waspProjectDir </> [relfile|src/utils.ts|]) contents
