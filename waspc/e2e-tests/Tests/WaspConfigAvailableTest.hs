module Tests.WaspConfigAvailableTest (waspConfigAvailableTest) where

import Control.Monad.Reader (ask)
import Data.Aeson ((.=))
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text.Encoding as TE
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
    waspCliInfo,
    waspCliInstall,
    waspCliNews,
    waspCliReinstall,
    waspCliStart,
    waspCliStartDb,
    waspCliStudio,
    waspCliTelemetry,
    waspCliTestClient,
    waspCliVersion,
    writeToFile,
  )
import StrongPath (relfile, (</>))
import Test (Test (..), TestCase (..))
import Wasp.Cli.Command.CreateNewProject.AvailableTemplates (tsMinimalStarterTemplate)

waspConfigAvailableTest :: Test
waspConfigAvailableTest =
  Test
    "wasp-config-available"
    [ TestCase
        "commands-requiring-wasp-config-fail-with-install-hint-when-missing"
        ( sequence
            [ createTestWaspProject tsMinimalStarterTemplate,
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
        "compile-fails-with-install-hint-when-wasp-config-version-mismatches-cli"
        ( sequence
            [ createTestWaspProject tsMinimalStarterTemplate,
              inTestWaspProjectDir
                [ corruptWaspConfigVersion,
                  assertCommandFailsWithInstallHint waspCliCompile
                ]
            ]
        ),
      TestCase
        "build-start-fails-with-install-hint-when-wasp-config-missing"
        -- `wasp build start` requires `.wasp/build` to exist before its
        -- `WaspConfigAvailable` check fires, so we build first, then nuke
        -- node_modules to reach the install-hint failure path.
        ( sequence
            [ createTestWaspProject tsMinimalStarterTemplate,
              inTestWaspProjectDir
                [ setWaspDbToPSQL,
                  waspCliBuild,
                  removeNodeModules,
                  assertCommandFailsWithInstallHint (waspCliBuildStart "")
                ]
            ]
        ),
      TestCase
        "commands-not-requiring-wasp-config-succeed-when-missing"
        ( sequence
            [ createTestWaspProject tsMinimalStarterTemplate,
              inTestWaspProjectDir $
                concatMap
                  (\command -> [removeNodeModules, command])
                  -- Project-scoped commands that don't gate on wasp-config.
                  [ waspCliClean,
                    waspCliInstall,
                    waspCliReinstall,
                    -- Project-agnostic commands. They don't read the project,
                    -- so they should be unaffected by wasp-config presence.
                    waspCliVersion,
                    waspCliCompletion,
                    waspCliTelemetry,
                    waspCliNews
                  ]
            ]
        )
    ]
  where
    -- Putting the project in a "wasp-config missing" state without invoking any wasp command.
    removeNodeModules :: ShellCommandBuilder WaspProjectContext ShellCommand
    removeNodeModules = return "rm -rf node_modules"

    corruptWaspConfigVersion :: ShellCommandBuilder WaspProjectContext ShellCommand
    corruptWaspConfigVersion = do
      context <- ask
      let waspConfigPackageJson = context.waspProjectDir </> [relfile|.wasp/wasp-config/package.json|]
      writeToFile waspConfigPackageJson corruptedPackageJsonContent
      where
        corruptedPackageJsonContent =
          TE.decodeUtf8 . LBS.toStrict . Aeson.encode $
            Aeson.object
              [ "name" .= ("wasp-config" :: String),
                "version" .= ("9.9.9" :: String)
              ]

    assertCommandFailsWithInstallHint ::
      ShellCommandBuilder WaspProjectContext ShellCommand ->
      ShellCommandBuilder WaspProjectContext ShellCommand
    assertCommandFailsWithInstallHint commandBuilder =
      -- Negate the wrapped command so the assertion holds when it fails (exit non-zero)
      -- AND the output contains the "Run `wasp install`" hint.
      assertCommandOutputContains (("! " ++) <$> commandBuilder) "wasp install"
