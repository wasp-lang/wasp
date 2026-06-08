module Tests.WaspInstallTest (waspInstallTest) where

import Control.Monad.Reader (ask)
import qualified Data.Text as T
import ShellCommands
  ( ShellCommand,
    ShellCommandBuilder,
    WaspProjectContext (..),
    assertDirectoryDoesNotExist,
    assertDirectoryExists,
    createTestWaspProject,
    inTestWaspProjectDir,
    waspCliClean,
    waspCliCompile,
    waspCliInstall,
    writeToFile,
    (~&&),
  )
import StrongPath (relfile, (</>))
import Test (Test (..), TestCase (..))
import Wasp.Cli.Command.CreateNewProject.AvailableTemplates (minimalStarterTemplate)
import Wasp.Project.Common (generatedAppDirInWaspProjectDir)

waspInstallTest :: Test
waspInstallTest =
  Test
    "wasp-install"
    [ TestCase
        "install-fails-outside-project"
        (return [waspCliInstallFails]),
      TestCase
        "install-restores-wasp-spec-after-clean"
        ( sequence
            [ createTestWaspProject minimalStarterTemplate,
              inTestWaspProjectDir
                [ waspCliClean,
                  return $ assertDirectoryDoesNotExist "node_modules",
                  waspCliInstall,
                  return $ assertSymlinkExists "node_modules/@wasp.sh/spec",
                  waspCliCompile
                ]
            ]
        ),
      TestCase
        "install-removes-stale-generated-workspaces"
        ( sequence
            [ createTestWaspProject minimalStarterTemplate,
              inTestWaspProjectDir
                [ waspCliClean,
                  createStaleGeneratedAppDir,
                  waspCliInstall,
                  return $ assertDirectoryDoesNotExist ".wasp/out",
                  return $ assertSymlinkExists "node_modules/@wasp.sh/spec"
                ]
            ]
        ),
      TestCase
        "install-keeps-current-generated-workspaces"
        ( sequence
            [ createTestWaspProject minimalStarterTemplate,
              inTestWaspProjectDir
                [ waspCliCompile,
                  return $ assertDirectoryExists ".wasp/out",
                  waspCliInstall,
                  return $ assertDirectoryExists ".wasp/out"
                ]
            ]
        )
    ]
  where
    waspCliInstallFails :: ShellCommand
    waspCliInstallFails = "! wasp-cli install"

    assertSymlinkExists :: FilePath -> ShellCommand
    assertSymlinkExists path = "[ -L '" ++ path ++ "' ]"

    createStaleGeneratedAppDir :: ShellCommandBuilder WaspProjectContext ShellCommand
    createStaleGeneratedAppDir = do
      context <- ask
      let generatedAppDir = context.waspProjectDir </> generatedAppDirInWaspProjectDir
      writeWaspInfo <- writeToFile (generatedAppDir </> [relfile|.waspinfo|]) staleWaspInfo
      writeServerPackageJson <- writeToFile (generatedAppDir </> [relfile|server/package.json|]) staleServerPackageJson
      return $ writeWaspInfo ~&& writeServerPackageJson

    staleWaspInfo :: T.Text
    staleWaspInfo =
      T.unlines
        [ "{",
          "  \"waspVersion\": \"0.23.0\",",
          "  \"generatedAt\": \"2025-01-01T00:00:00Z\",",
          "  \"buildType\": \"Development\"",
          "}"
        ]

    staleServerPackageJson :: T.Text
    staleServerPackageJson =
      T.unlines
        [ "{",
          "  \"name\": \"stale-generated-server\",",
          "  \"version\": \"0.23.0\",",
          "  \"dependencies\": {",
          "    \"@wasp.sh/lib-auth\": \"file:../libs/@wasp_sh_lib-auth-0.23.0.tgz\"",
          "  }",
          "}"
        ]
