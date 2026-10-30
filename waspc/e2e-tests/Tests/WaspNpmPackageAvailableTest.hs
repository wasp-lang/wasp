module Tests.WaspNpmPackageAvailableTest (waspNpmPackageAvailableTest) where

import Control.Monad.Reader (ask)
import ShellCommands
  ( ShellCommand,
    ShellCommandBuilder,
    WaspProjectContext (..),
    assertCommandOutputContains,
    createTestWaspProject,
    inTestWaspProjectDir,
    setWaspDbToPSQL,
    waspCliBuild,
    waspCliBuildStart,
    waspCliClean,
    waspCliCompile,
    waspCliCompletion,
    waspCliDbReset,
    waspCliDeploy,
    waspCliDeps,
    waspCliDockerfile,
    waspCliNews,
    waspCliShowBuild,
    waspCliShowSpec,
    waspCliStart,
    waspCliStartDb,
    waspCliStudio,
    waspCliTelemetry,
    waspCliTestClient,
    waspCliVersion,
  )
import StrongPath (fromAbsDir, reldir, (</>))
import Test (Test (..), TestCase (..))
import Wasp.Cli.Command.CreateNewProject.AvailableTemplates (minimalStarterTemplate)
import Wasp.Util.Terminal (styleCode)

waspNpmPackageAvailableTest :: Test
waspNpmPackageAvailableTest =
  Test
    "wasp-npm-package-available"
    [ TestCase
        "commands-requiring-wasp-npm-package-fail-with-install-hint-when-missing"
        ( sequence
            [ createTestWaspProject minimalStarterTemplate,
              inTestWaspProjectDir $
                removeNodeModules
                  : map
                    assertCommandFailsWithInstallHint
                    [ waspCliCompile,
                      waspCliBuild,
                      waspCliShowSpec,
                      waspCliDeps,
                      waspCliDockerfile,
                      waspCliStudio,
                      waspCliDbReset,
                      waspCliDeploy ["fly", "setup"],
                      waspCliStartDb,
                      waspCliStart,
                      waspCliTestClient []
                    ]
            ]
        ),
      TestCase
        "compile-fails-with-hint-when-wasp-npm-package-version-mismatches-cli"
        ( sequence
            [ createTestWaspProject minimalStarterTemplate,
              inTestWaspProjectDir
                [ corruptWaspNpmPackageVersion,
                  assertCommandFailsWithVersionMismatchHint waspCliCompile
                ]
            ]
        ),
      TestCase
        "compile-fails-with-hint-when-wasp-npm-package-is-not-a-dependency"
        ( sequence
            [ createTestWaspProject minimalStarterTemplate,
              inTestWaspProjectDir
                [ removeWaspNpmPackageDependency,
                  assertCommandFailsWithNotADependencyHint waspCliCompile
                ]
            ]
        ),
      TestCase
        "build-start-fails-with-install-hint-when-wasp-npm-package-missing"
        -- `wasp build start` requires `.wasp/build` to exist before its
        -- `WaspNpmPackageAvailable` check fires, so we build first, then nuke
        -- node_modules to reach the install-hint failure path.
        ( sequence
            [ createTestWaspProject minimalStarterTemplate,
              inTestWaspProjectDir
                [ setWaspDbToPSQL,
                  waspCliBuild,
                  removeNodeModules,
                  assertCommandFailsWithInstallHint (waspCliBuildStart "")
                ]
            ]
        ),
      TestCase
        "commands-not-requiring-wasp-npm-package-succeed-when-missing"
        ( sequence
            [ createTestWaspProject minimalStarterTemplate,
              inTestWaspProjectDir $
                concatMap
                  (\command -> [removeNodeModules, command])
                  -- Project-scoped commands that don't require @wasp/wasp to be installed.
                  [ waspCliClean,
                    waspCliShowBuild,
                    -- Project-agnostic commands. They don't read the project,
                    -- so they should be unaffected by @wasp/wasp presence.
                    waspCliVersion,
                    waspCliCompletion,
                    waspCliTelemetry,
                    waspCliNews
                  ]
            ]
        )
    ]
  where
    -- Putting the project in a "@wasp/wasp missing" state without invoking any wasp command.
    removeNodeModules :: ShellCommandBuilder WaspProjectContext ShellCommand
    removeNodeModules = return "rm -rf node_modules"

    -- A project from before Wasp 0.27, which never declared @wasp/wasp.
    removeWaspNpmPackageDependency :: ShellCommandBuilder WaspProjectContext ShellCommand
    removeWaspNpmPackageDependency = return "npm pkg delete dependencies.@wasp/wasp && rm -rf node_modules/@wasp/wasp"

    corruptWaspNpmPackageVersion :: ShellCommandBuilder WaspProjectContext ShellCommand
    corruptWaspNpmPackageVersion = do
      context <- ask
      let waspNpmPackageDir = context.waspProjectDir </> [reldir|node_modules/@wasp/wasp|]
      return $ "(cd " ++ fromAbsDir waspNpmPackageDir ++ " && npm pkg set version=9.9.9)"

    assertCommandFailsWithInstallHint ::
      ShellCommandBuilder WaspProjectContext ShellCommand ->
      ShellCommandBuilder WaspProjectContext ShellCommand
    assertCommandFailsWithInstallHint commandBuilder =
      -- Negate the wrapped command so the assertion holds when it fails (exit non-zero)
      -- AND the output contains the "Run `npm install`" hint.
      assertCommandOutputContains (("! " ++) <$> commandBuilder) ("Run " ++ styleCode "npm install")

    assertCommandFailsWithNotADependencyHint ::
      ShellCommandBuilder WaspProjectContext ShellCommand ->
      ShellCommandBuilder WaspProjectContext ShellCommand
    assertCommandFailsWithNotADependencyHint commandBuilder =
      assertCommandOutputContains (("! " ++) <$> commandBuilder) "Wasp is not a dependency of this project"

    assertCommandFailsWithVersionMismatchHint ::
      ShellCommandBuilder WaspProjectContext ShellCommand ->
      ShellCommandBuilder WaspProjectContext ShellCommand
    assertCommandFailsWithVersionMismatchHint commandBuilder =
      assertCommandOutputContains (("! " ++) <$> commandBuilder) "Wasp version mismatch"
