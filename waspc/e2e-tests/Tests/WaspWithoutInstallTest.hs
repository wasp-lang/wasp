module Tests.WaspWithoutInstallTest (waspWithoutInstallTest) where

import ShellCommands
  ( ShellCommand,
    ShellCommandBuilder,
    WaspProjectContext (..),
    createTestWaspProject,
    inTestWaspProjectDir,
    setWaspDbToPSQL,
    waspCliBuild,
    waspCliCompile,
    waspCliDeps,
    waspCliDockerfile,
    waspCliShowSpecJson,
  )
import Test (Test (..), TestCase (..))
import Wasp.Cli.Command.CreateNewProject.AvailableTemplates (minimalStarterTemplate)

-- | Commands that read the Wasp spec work on a project whose dependencies
-- aren't installed (e.g., a fresh clone), without running `wasp install` first.
waspWithoutInstallTest :: Test
waspWithoutInstallTest =
  Test
    "wasp-without-install"
    [ TestCase
        "spec-commands-succeed-on-fresh-clone"
        ( sequence
            [ createTestWaspProject minimalStarterTemplate,
              inTestWaspProjectDir $
                concatMap
                  (\command -> [makeProjectLookFreshlyCloned, command])
                  [ waspCliCompile,
                    waspCliDeps,
                    waspCliShowSpecJson,
                    waspCliDockerfile
                  ]
            ]
        ),
      TestCase
        "build-succeeds-on-fresh-clone"
        ( sequence
            [ createTestWaspProject minimalStarterTemplate,
              inTestWaspProjectDir
                [ setWaspDbToPSQL,
                  makeProjectLookFreshlyCloned,
                  waspCliBuild
                ]
            ]
        ),
      TestCase
        "compile-sets-up-editor-types-on-fresh-clone"
        ( sequence
            [ createTestWaspProject minimalStarterTemplate,
              inTestWaspProjectDir
                [ makeProjectLookFreshlyCloned,
                  waspCliCompile,
                  return $ assertFileExists ".wasp/spec/package.json",
                  return $ assertSymlinkExists "node_modules/@wasp.sh/spec"
                ]
            ]
        ),
      TestCase
        "compile-keeps-package-lock-unchanged-on-fresh-clone"
        ( sequence
            [ createTestWaspProject minimalStarterTemplate,
              inTestWaspProjectDir
                [ -- The lock of a compiled project is what a user commits.
                  waspCliCompile,
                  return $ "cp package-lock.json " ++ committedPackageLockPath,
                  makeProjectLookFreshlyCloned,
                  waspCliCompile,
                  return $ "cmp package-lock.json " ++ committedPackageLockPath
                ]
            ]
        )
    ]
  where
    -- Both directories are gitignored, so a fresh clone has neither.
    makeProjectLookFreshlyCloned :: ShellCommandBuilder WaspProjectContext ShellCommand
    makeProjectLookFreshlyCloned = return "rm -rf node_modules .wasp"

    -- Outside of the project dir, so Wasp doesn't see it.
    committedPackageLockPath :: FilePath
    committedPackageLockPath = "../committed-package-lock.json"

    assertFileExists :: FilePath -> ShellCommand
    assertFileExists filePath = "[ -f '" ++ filePath ++ "' ]"

    assertSymlinkExists :: FilePath -> ShellCommand
    assertSymlinkExists path = "[ -L '" ++ path ++ "' ]"
