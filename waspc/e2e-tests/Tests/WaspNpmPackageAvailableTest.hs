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
    waspCliVersion,
  )
import StrongPath (fromAbsDir, reldir, (</>))
import Test (Test (..), TestCase (..))
import Wasp.Cli.Command.CreateNewProject.AvailableTemplates (minimalStarterTemplate)
import Wasp.Util.Terminal (styleCode)
import Wasp.Version (waspVersion)

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
                    (assertCommandFailsWith installHint)
                    [ waspCliCompile,
                      waspCliBuild,
                      waspCliShowSpec,
                      waspCliDeps,
                      waspCliDockerfile,
                      waspCliStudio,
                      waspCliDbReset,
                      waspCliDeploy ["fly", "setup"],
                      waspCliStartDb,
                      waspCliStart
                    ]
            ]
        ),
      TestCase
        "compile-fails-with-install-hint-when-installed-wasp-npm-package-is-stale"
        ( sequence
            [ createTestWaspProject minimalStarterTemplate,
              inTestWaspProjectDir
                [ setDeclaredWaspNpmPackageVersion (show waspVersion),
                  setInstalledWaspNpmPackageVersion nonexistentWaspVersion,
                  assertCommandFailsWith installHint waspCliCompile
                ]
            ]
        ),
      TestCase
        "compile-fails-with-hint-when-wasp-npm-package-mismatches-directly-run-cli"
        ( sequence
            [ createTestWaspProject minimalStarterTemplate,
              inTestWaspProjectDir
                [ setDeclaredWaspNpmPackageVersion nonexistentWaspVersion,
                  setInstalledWaspNpmPackageVersion nonexistentWaspVersion,
                  assertCommandFailsWith "directly, which can't switch to the project's version" waspCliCompile
                ]
            ]
        ),
      TestCase
        "compile-fails-with-hint-when-wasp-npm-package-mismatches-npm-cli"
        ( sequence
            [ createTestWaspProject minimalStarterTemplate,
              inTestWaspProjectDir
                [ setDeclaredWaspNpmPackageVersion nonexistentWaspVersion,
                  setInstalledWaspNpmPackageVersion nonexistentWaspVersion,
                  assertCommandFailsWith "to install them together" (runAsNpmCli waspCliCompile)
                ]
            ]
        ),
      TestCase
        "compile-fails-with-hint-when-wasp-npm-package-is-not-a-dependency"
        ( sequence
            [ createTestWaspProject minimalStarterTemplate,
              inTestWaspProjectDir
                [ removeWaspNpmPackageDependency,
                  assertCommandFailsWith "Wasp is not a dependency of this project" waspCliCompile
                ]
            ]
        ),
      TestCase
        "compile-fails-with-hint-when-wasp-npm-package-is-a-dev-dependency"
        ( sequence
            [ createTestWaspProject minimalStarterTemplate,
              inTestWaspProjectDir
                [ moveWaspNpmPackageToDevDependencies,
                  assertCommandFailsWith "Wasp is a dev dependency of this project" waspCliCompile
                ]
            ]
        ),
      TestCase
        "build-start-fails-with-install-hint-when-wasp-npm-package-missing"
        -- `wasp build start` checks that `.wasp/build` exists before it requires
        -- `@wasp/wasp`, so we build first and remove node_modules afterwards.
        ( sequence
            [ createTestWaspProject minimalStarterTemplate,
              inTestWaspProjectDir
                [ setWaspDbToPSQL,
                  waspCliBuild,
                  removeNodeModules,
                  assertCommandFailsWith installHint (waspCliBuildStart "")
                ]
            ]
        ),
      TestCase
        "commands-not-requiring-wasp-npm-package-succeed-when-missing"
        ( sequence
            [ createTestWaspProject minimalStarterTemplate,
              inTestWaspProjectDir $
                removeNodeModules
                  -- Project-scoped commands that don't require @wasp/wasp to be installed.
                  : [ waspCliClean,
                      waspCliShowBuild,
                      -- Project-agnostic commands.
                      waspCliVersion,
                      waspCliCompletion,
                      waspCliTelemetry,
                      waspCliNews
                    ]
            ]
        )
    ]
  where
    removeNodeModules :: ShellCommandBuilder WaspProjectContext ShellCommand
    removeNodeModules = return "rm -rf node_modules"

    -- A project from before Wasp 0.27, which never declared @wasp/wasp.
    removeWaspNpmPackageDependency :: ShellCommandBuilder WaspProjectContext ShellCommand
    removeWaspNpmPackageDependency = return "npm pkg delete dependencies.@wasp/wasp"

    moveWaspNpmPackageToDevDependencies :: ShellCommandBuilder WaspProjectContext ShellCommand
    moveWaspNpmPackageToDevDependencies =
      return
        "npm pkg set \"devDependencies.@wasp/wasp=$(npm pkg get dependencies.@wasp/wasp | jq -r .)\" && npm pkg delete dependencies.@wasp/wasp"

    -- No Wasp release will ever have this version, so the CLI never matches it.
    nonexistentWaspVersion :: String
    nonexistentWaspVersion = "0.0.0-e2e-nonexistent"

    setDeclaredWaspNpmPackageVersion :: String -> ShellCommandBuilder WaspProjectContext ShellCommand
    setDeclaredWaspNpmPackageVersion version = return $ "npm pkg set dependencies.@wasp/wasp=" ++ version

    setInstalledWaspNpmPackageVersion :: String -> ShellCommandBuilder WaspProjectContext ShellCommand
    setInstalledWaspNpmPackageVersion version = do
      context <- ask
      let waspNpmPackageDir = context.waspProjectDir </> [reldir|node_modules/@wasp/wasp|]
      return $ "(cd " ++ fromAbsDir waspNpmPackageDir ++ " && npm pkg set version=" ++ version ++ ")"

    -- Like the npm \@wasp/cli, which tells the binary its version.
    runAsNpmCli ::
      ShellCommandBuilder WaspProjectContext ShellCommand ->
      ShellCommandBuilder WaspProjectContext ShellCommand
    runAsNpmCli = fmap (("WASP_CLI_NPM_PACKAGE_VERSION=" ++ show waspVersion ++ " ") ++)

    installHint :: String
    installHint = "Run " ++ styleCode "npm install"

    -- Negates the command, so the assertion holds when it fails (exits non-zero)
    -- and its output contains the expected text.
    assertCommandFailsWith ::
      String ->
      ShellCommandBuilder WaspProjectContext ShellCommand ->
      ShellCommandBuilder WaspProjectContext ShellCommand
    assertCommandFailsWith expectedOutput commandBuilder =
      assertCommandOutputContains (("! " ++) <$> commandBuilder) expectedOutput
