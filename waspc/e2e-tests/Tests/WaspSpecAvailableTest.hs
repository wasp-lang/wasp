module Tests.WaspSpecAvailableTest (waspSpecAvailableTest) where

import Command (Command, cmd, inRelativeDir)
import Context (WaspProjectContext)
import Step (Step)
import Steps
  ( assertCommandFailsWithOutputContaining,
    createTestWaspProject,
    inTestWaspProjectDir,
    removeDirRecursively,
    runCommand,
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
    waspCliInfo,
    waspCliInstall,
    waspCliNews,
    waspCliStart,
    waspCliStartDb,
    waspCliStudio,
    waspCliTelemetry,
    waspCliTestClient,
    waspCliVersion,
  )
import StrongPath (reldir)
import Test (Test (..), TestCase (..))
import Wasp.Cli.Command.CreateNewProject.AvailableTemplates (minimalStarterTemplate)
import Wasp.Util.Terminal (styleCode)

waspSpecAvailableTest :: Test
waspSpecAvailableTest =
  Test
    "wasp-spec-available"
    [ TestCase
        "commands-requiring-wasp-spec-fail-with-install-hint-when-missing"
        ( sequence
            [ createTestWaspProject minimalStarterTemplate,
              inTestWaspProjectDir $
                removeNodeModules
                  : map
                    assertCommandFailsWithInstallHint
                    [ waspCliCompile,
                      waspCliBuild,
                      waspCliInfo,
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
        "compile-fails-with-install-hint-when-wasp-spec-version-mismatches-cli"
        ( sequence
            [ createTestWaspProject minimalStarterTemplate,
              inTestWaspProjectDir
                [ corruptWaspSpecVersion,
                  assertCommandFailsWithInstallHint waspCliCompile
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
                  runCommand waspCliBuild,
                  removeNodeModules,
                  assertCommandFailsWithInstallHint (waspCliBuildStart [])
                ]
            ]
        ),
      TestCase
        "commands-not-requiring-wasp-spec-succeed-when-missing"
        ( sequence
            [ createTestWaspProject minimalStarterTemplate,
              inTestWaspProjectDir $
                concatMap
                  (\command -> [removeNodeModules, runCommand command])
                  -- Project-scoped commands that don't require wasp-spec to be installed.
                  [ waspCliClean,
                    waspCliInstall,
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
    removeNodeModules :: Step WaspProjectContext ()
    removeNodeModules = removeDirRecursively "node_modules"

    corruptWaspSpecVersion :: Step WaspProjectContext ()
    corruptWaspSpecVersion =
      runCommand $ inRelativeDir [reldir|.wasp/spec|] $ cmd "npm" ["pkg", "set", "version=9.9.9"]

    assertCommandFailsWithInstallHint :: Command -> Step WaspProjectContext ()
    assertCommandFailsWithInstallHint command =
      -- The assertion holds when the command fails (exits non-zero)
      -- AND its output contains the "Run `wasp install`" hint.
      assertCommandFailsWithOutputContaining command ("Run " ++ styleCode "wasp install")
