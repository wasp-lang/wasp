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
import StrongPath (fromAbsDir, reldir, (</>))
import Test (Test (..), TestCase (..))
import Wasp.Cli.Command.CreateNewProject.AvailableTemplates (minimalStarterTemplate)
import Wasp.Util.Terminal (styleCode)
import Wasp.Version (waspVersion)

waspSpecAvailableTest :: Test
waspSpecAvailableTest =
  Test
    "wasp-spec-available"
    [ TestCase
        "lock-free-commands-fail-with-install-hint-when-wasp-spec-missing"
        -- Commands that don't hold the project lock can't install dependencies
        -- (a `wasp start` might be using them), so they fail fast instead.
        ( sequence
            [ createTestWaspProject minimalStarterTemplate,
              inTestWaspProjectDir $
                removeNodeModules
                  : map
                    assertCommandFailsWithInstallHint
                    [ waspCliShowSpec,
                      waspCliDeps,
                      waspCliDockerfile,
                      waspCliStudio,
                      waspCliDeploy ["fly", "setup"],
                      waspCliStartDb
                    ]
            ]
        ),
      TestCase
        "lock-free-command-fails-with-install-hint-when-wasp-spec-version-mismatches-cli"
        ( sequence
            [ createTestWaspProject minimalStarterTemplate,
              inTestWaspProjectDir
                [ corruptWaspSpecVersion,
                  assertCommandFailsWithInstallHint waspCliDeps
                ]
            ]
        ),
      TestCase
        "lock-holding-commands-install-missing-wasp-spec"
        -- `wasp start` and `wasp test client` install through the same
        -- `compile` as `wasp compile`, but they don't terminate, so we don't
        -- run them here.
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
        "heal-keeps-package-lock-unchanged"
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
    -- Putting the project in a "wasp-spec missing" state without invoking any wasp command.
    removeNodeModules :: ShellCommandBuilder WaspProjectContext ShellCommand
    removeNodeModules = return "rm -rf node_modules"

    corruptWaspSpecVersion :: ShellCommandBuilder WaspProjectContext ShellCommand
    corruptWaspSpecVersion = do
      context <- ask
      let waspSpecDir = context.waspProjectDir </> [reldir|.wasp/spec|]
      return $ "(cd " ++ fromAbsDir waspSpecDir ++ " && npm pkg set version=9.9.9)"

    assertWaspSpecVersionMatchesCli :: ShellCommand
    assertWaspSpecVersionMatchesCli =
      "[ \"$(cd node_modules/@wasp.sh/spec && npm pkg get version)\" = '\"" ++ show waspVersion ++ "\"' ]"

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
        -- npm links the `file:.wasp/spec` dependency.
        return "[ -L node_modules/@wasp.sh/spec ]"
      ]

    installingDependenciesMessage :: String
    installingDependenciesMessage = "Installing missing or outdated project dependencies..."
