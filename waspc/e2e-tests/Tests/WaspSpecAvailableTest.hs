module Tests.WaspSpecAvailableTest (waspSpecAvailableTest) where

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

waspSpecAvailableTest :: Test
waspSpecAvailableTest =
  Test
    "wasp-spec-available"
    [ TestCase
        "commands-requiring-wasp-spec-fail-with-install-hint-when-missing"
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
        "compile-fails-with-install-hint-when-wasp-spec-version-mismatches-cli"
        ( sequence
            [ createTestWaspProject tsMinimalStarterTemplate,
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
        "commands-not-requiring-wasp-spec-succeed-when-missing"
        ( sequence
            [ createTestWaspProject tsMinimalStarterTemplate,
              inTestWaspProjectDir $
                concatMap
                  (\command -> [removeNodeModules, command])
                  -- Project-scoped commands that don't gate on wasp-spec.
                  [ waspCliClean,
                    waspCliInstall,
                    waspCliReinstall,
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
      let waspSpecPackageJson = context.waspProjectDir </> [relfile|.wasp/spec/package.json|]
      writeToFile waspSpecPackageJson corruptedPackageJsonContent
      where
        corruptedPackageJsonContent =
          TE.decodeUtf8 . LBS.toStrict . Aeson.encode $
            Aeson.object
              [ "name" .= ("@wasp.sh/spec" :: String),
                "version" .= ("9.9.9" :: String)
              ]

    assertCommandFailsWithInstallHint ::
      ShellCommandBuilder WaspProjectContext ShellCommand ->
      ShellCommandBuilder WaspProjectContext ShellCommand
    assertCommandFailsWithInstallHint commandBuilder =
      -- Negate the wrapped command so the assertion holds when it fails (exit non-zero)
      -- AND the output contains the "Run `wasp install`" hint.
      assertCommandOutputContains (("! " ++) <$> commandBuilder) "wasp install"
