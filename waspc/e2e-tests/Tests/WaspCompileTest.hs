module Tests.WaspCompileTest (waspCompileTest) where

import Control.Monad.Reader (ask)
import qualified Data.Text as T
import NeatInterpolation (trimming)
import ShellCommands
  ( ShellCommand,
    ShellCommandBuilder,
    WaspProjectContext (..),
    appendToFile,
    assertCommandOutputContains,
    createTestWaspProject,
    inTestWaspProjectDir,
    replaceMainWaspTsFile,
    waspCliCompile,
    writeToFile,
  )
import StrongPath (relfile, (</>))
import Test (Test (..), TestCase (..))
import Wasp.Cli.Command.CreateNewProject.AvailableTemplates (minimalStarterTemplate)
import Wasp.Version (waspVersion)

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
      TestCase
        "fail-on-client-code-type-error"
        ( sequence
            [ createTestWaspProject minimalStarterTemplate,
              inTestWaspProjectDir
                [ appendToFile "src/MainPage.tsx" missingSideEffectImport,
                  assertCommandOutputContains (return waspCliCompileFails) userCodeTypeCheckFailure
                ]
            ]
        ),
      TestCase
        "fail-on-server-code-type-error"
        ( sequence
            [ createTestWaspProject minimalStarterTemplate,
              inTestWaspProjectDir
                [ replaceMainWaspTsFile mainWaspTsWithServerSetup,
                  writeServerSetupTs,
                  assertCommandOutputContains (return waspCliCompileFails) userCodeTypeCheckFailure
                ]
            ]
        ),
      TestCase
        "fail-on-unreferenced-user-error-and-recover"
        ( sequence
            [ createTestWaspProject minimalStarterTemplate,
              inTestWaspProjectDir
                [ writeTypeCheckTs "export const count: number = 'wrong'\n",
                  assertCommandOutputContains (return waspCliCompileFails) userCodeTypeCheckFailure,
                  writeTypeCheckTs "export const count: number = 1\n",
                  waspCliCompile
                ]
            ]
        )
    ]
  where
    waspCliCompileFails :: ShellCommand
    waspCliCompileFails = "! $WASP_CLI_CMD compile"

    assertDirectoryExists :: FilePath -> ShellCommand
    assertDirectoryExists dirFilePath = "[ -d '" ++ dirFilePath ++ "' ]"

    missingSideEffectImport :: T.Text
    missingSideEffectImport = "import './missing-side-effect-import'"

    userCodeTypeCheckFailure :: String
    userCodeTypeCheckFailure = "User code type-check failed with exit code:"

    mainWaspTsWithServerSetup :: T.Text
    mainWaspTsWithServerSetup =
      [trimming|
        import { app, page, route } from "@wasp.sh/spec"
        import { MainPage } from "./src/MainPage" with { type: "ref" }
        import { serverSetup } from "./src/serverSetup" with { type: "ref" }

        export default app({
          name: "typeCheckTest",
          title: "Type check test",
          wasp: { version: "$textWaspVersion" },
          server: { setupFn: serverSetup },
          spec: [route("RootRoute", "/", page(MainPage))]
        })
      |]

    textWaspVersion :: T.Text
    textWaspVersion = T.pack . show $ waspVersion

    writeServerSetupTs :: ShellCommandBuilder WaspProjectContext ShellCommand
    writeServerSetupTs = do
      context <- ask
      writeToFile
        (context.waspProjectDir </> [relfile|src/serverSetup.ts|])
        [trimming|
          $missingSideEffectImport

          export const serverSetup = () => {}
        |]

    writeTypeCheckTs :: T.Text -> ShellCommandBuilder WaspProjectContext ShellCommand
    writeTypeCheckTs contents = do
      context <- ask
      writeToFile (context.waspProjectDir </> [relfile|src/typeCheck.ts|]) contents
