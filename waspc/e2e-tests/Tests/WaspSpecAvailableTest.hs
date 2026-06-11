module Tests.WaspSpecAvailableTest (waspSpecAvailableTest) where

import Command (Command, cmd, inRelativeDir)
import Context (WaspProjectContext)
import Control.Monad (forM_)
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
    [ TestCase "commands-requiring-wasp-spec-fail-with-install-hint-when-missing" $ do
        createTestWaspProject minimalStarterTemplate
        inTestWaspProjectDir $ do
          removeNodeModules
          mapM_
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
            ],
      TestCase "compile-fails-with-install-hint-when-wasp-spec-version-mismatches-cli" $ do
        createTestWaspProject minimalStarterTemplate
        inTestWaspProjectDir $ do
          corruptWaspSpecVersion
          assertCommandFailsWithInstallHint waspCliCompile,
      TestCase "build-start-fails-with-install-hint-when-wasp-spec-missing" $ do
        -- `wasp build start` requires `.wasp/build` to exist before its
        -- `WaspSpecAvailable` check fires, so we build first, then nuke
        -- node_modules to reach the install-hint failure path.
        createTestWaspProject minimalStarterTemplate
        inTestWaspProjectDir $ do
          setWaspDbToPSQL
          runCommand waspCliBuild
          removeNodeModules
          assertCommandFailsWithInstallHint (waspCliBuildStart []),
      TestCase "commands-not-requiring-wasp-spec-succeed-when-missing" $ do
        createTestWaspProject minimalStarterTemplate
        inTestWaspProjectDir
          $ forM_
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
          $ \command -> do
            removeNodeModules
            runCommand command
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
