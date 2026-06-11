module Tests.WaspInstallTest (waspInstallTest) where

import Steps
  ( assertDirDoesNotExist,
    assertSymlinkExists,
    createTestWaspProject,
    inTestWaspProjectDir,
    runCommand,
    runCommandExpectingFailure,
    waspCliClean,
    waspCliCompile,
    waspCliInstall,
  )
import Test (Test (..), TestCase (..))
import Wasp.Cli.Command.CreateNewProject.AvailableTemplates (minimalStarterTemplate)

waspInstallTest :: Test
waspInstallTest =
  Test
    "wasp-install"
    [ TestCase "install-fails-outside-project" $
        runCommandExpectingFailure waspCliInstall,
      TestCase "install-restores-wasp-spec-after-clean" $ do
        createTestWaspProject minimalStarterTemplate
        inTestWaspProjectDir $ do
          runCommand waspCliClean
          assertDirDoesNotExist "node_modules"
          runCommand waspCliInstall
          assertSymlinkExists "node_modules/@wasp.sh/spec"
          runCommand waspCliCompile
    ]
