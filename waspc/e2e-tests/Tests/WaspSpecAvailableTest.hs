module Tests.WaspSpecAvailableTest (waspSpecAvailableTest) where

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
    waspCliInstall,
    waspCliNews,
    waspCliShowBuild,
    waspCliShowSpec,
    waspCliStartDb,
    waspCliStudio,
    waspCliTelemetry,
    waspCliVersion,
  )
import StrongPath (fromAbsDir, fromRelDir, reldir, (</>))
import Test (Test (..), TestCase (..))
import Wasp.Cli.Command.CreateNewProject.AvailableTemplates (minimalStarterTemplate)
import Wasp.Cli.Command.Install (installingDependenciesMessage)
import Wasp.NodePackageFFI (InstallablePackage (WaspSpecPackage), getInstallablePackageName)
import Wasp.Project.Common (nodeModulesDirInWaspProjectDir)
import Wasp.Util.Terminal (styleCode)
import Wasp.Version (waspVersion)

waspSpecAvailableTest :: Test
waspSpecAvailableTest =
  Test
    "wasp-spec-available"
    [ TestCase
        "lock-free-commands-fail-with-install-hint-when-wasp-spec-missing"
        -- These commands don't hold the project lock, so they don't install and
        -- fail with the install hint instead.
        ( sequence
            [ createTestWaspProject minimalStarterTemplate,
              inTestWaspProjectDir $
                removeNodeModules : map assertCommandFailsWithInstallHint lockFreeCommandsRequiringWaspSpec
            ]
        ),
      TestCase
        "lock-free-commands-fail-with-install-hint-when-wasp-spec-version-mismatches-cli"
        ( sequence
            [ createTestWaspProject minimalStarterTemplate,
              inTestWaspProjectDir $
                corruptWaspSpecVersion : map assertCommandFailsWithInstallHint lockFreeCommandsRequiringWaspSpec
            ]
        ),
      TestCase
        "lock-holding-commands-install-missing-wasp-spec"
        -- `wasp start` and `wasp test client` install through `compile` too,
        -- but they don't terminate, so we don't run them here.
        -- FIXME: Cover them once the e2e suite supports long-running processes:
        -- https://github.com/wasp-lang/wasp/issues/4601
        ( sequence
            [ createTestWaspProject minimalStarterTemplate,
              inTestWaspProjectDir $
                concat
                  [ assertCommandInstallsMissingWaspSpec waspCliCompile,
                    assertCommandInstallsMissingWaspSpec waspCliDbReset,
                    -- `wasp build` doesn't support SQLite.
                    [setWaspDbToPSQL],
                    assertCommandInstallsMissingWaspSpec waspCliBuild
                  ]
            ]
        ),
      TestCase
        "compile-reinstalls-wasp-spec-when-its-version-mismatches-cli"
        ( sequence
            [ createTestWaspProject minimalStarterTemplate,
              inTestWaspProjectDir
                [ corruptWaspSpecVersion,
                  assertCommandOutputContains waspCliCompile installingDependenciesMessage,
                  return assertWaspSpecVersionMatchesCli
                ]
            ]
        ),
      TestCase
        "compile-keeps-package-lock-unchanged-when-installing"
        ( sequence
            [ createTestWaspProject minimalStarterTemplate,
              inTestWaspProjectDir
                [ waspCliCompile,
                  return "cp package-lock.json package-lock.json.before",
                  return "rm -rf node_modules .wasp",
                  assertCommandOutputContains waspCliCompile installingDependenciesMessage,
                  return "cmp package-lock.json.before package-lock.json"
                ]
            ]
        ),
      TestCase
        "compile-skips-install-when-wasp-spec-is-installed-and-up-to-date"
        ( sequence
            [ createTestWaspProject minimalStarterTemplate,
              inTestWaspProjectDir
                [ assertCommandSkipsInstall waspCliCompile
                ]
            ]
        ),
      TestCase
        "build-start-fails-with-install-hint-when-wasp-spec-missing"
        -- `wasp build start` requires `.wasp/build` to exist before its
        -- `WaspSpecAvailable` check fires, so we build first, then nuke
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
        "commands-not-requiring-wasp-spec-succeed-when-missing"
        ( sequence
            [ createTestWaspProject minimalStarterTemplate,
              inTestWaspProjectDir $
                concatMap
                  (\command -> [removeNodeModules, command])
                  -- Project-scoped commands that don't require wasp-spec to be installed.
                  [ waspCliClean,
                    waspCliInstall,
                    waspCliShowBuild,
                    -- Project-agnostic commands. They don't read the project,
                    -- so they should be unaffected by wasp-spec presence.
                    waspCliVersion,
                    waspCliCompletion,
                    waspCliTelemetry,
                    waspCliNews
                  ]
            ]
        )
    ]
  where
    lockFreeCommandsRequiringWaspSpec :: [ShellCommandBuilder WaspProjectContext ShellCommand]
    lockFreeCommandsRequiringWaspSpec =
      [ waspCliShowSpec,
        waspCliDeps,
        waspCliDockerfile,
        waspCliStudio,
        waspCliDeploy ["fly", "setup"],
        waspCliStartDb
      ]

    -- Putting the project in a "wasp-spec missing" state without invoking any wasp command.
    removeNodeModules :: ShellCommandBuilder WaspProjectContext ShellCommand
    removeNodeModules = return "rm -rf node_modules"

    corruptWaspSpecVersion :: ShellCommandBuilder WaspProjectContext ShellCommand
    corruptWaspSpecVersion = do
      context <- ask
      let waspSpecDir = context.waspProjectDir </> [reldir|.wasp/spec|]
      return $ "(cd " ++ fromAbsDir waspSpecDir ++ " && npm pkg set version=9.9.9)"

    assertCommandFailsWithInstallHint ::
      ShellCommandBuilder WaspProjectContext ShellCommand ->
      ShellCommandBuilder WaspProjectContext ShellCommand
    assertCommandFailsWithInstallHint commandBuilder =
      -- Negate the wrapped command so the assertion holds when it fails (exit non-zero)
      -- AND the output contains the "Run `wasp install`" hint.
      assertCommandOutputContains (("! " ++) <$> commandBuilder) ("Run " ++ styleCode "wasp install")

    assertCommandInstallsMissingWaspSpec ::
      ShellCommandBuilder WaspProjectContext ShellCommand ->
      [ShellCommandBuilder WaspProjectContext ShellCommand]
    assertCommandInstallsMissingWaspSpec commandBuilder =
      [ removeNodeModules,
        assertCommandOutputContains commandBuilder installingDependenciesMessage,
        return assertWaspSpecVersionMatchesCli
      ]

    assertWaspSpecVersionMatchesCli :: ShellCommand
    assertWaspSpecVersionMatchesCli =
      "[ \"$(cd " ++ installedWaspSpecDir ++ " && npm pkg get version)\" = '\"" ++ show waspVersion ++ "\"' ]"
      where
        installedWaspSpecDir = fromRelDir nodeModulesDirInWaspProjectDir ++ getInstallablePackageName WaspSpecPackage

    assertCommandSkipsInstall ::
      ShellCommandBuilder WaspProjectContext ShellCommand ->
      ShellCommandBuilder WaspProjectContext ShellCommand
    assertCommandSkipsInstall commandBuilder = do
      command <- commandBuilder
      let logFile = ".wasp-e2e-output.log"
      return $ command ++ " > " ++ logFile ++ " 2>&1 && ! grep -qF '" ++ installingDependenciesMessage ++ "' " ++ logFile
