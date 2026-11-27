module Tests.WaspCleanTest (test_waspClean) where

import E2E
import qualified StrongPath as SP
import Wasp.Cli.Command.CreateNewProject.AvailableTemplates (minimalStarterTemplate)
import Wasp.Project.Common (dotWaspDirInWaspProjectDir)

test_waspClean :: TestTree
test_waspClean =
  testGroup
    "wasp-clean"
    [ test "fail-outside-project" $ waspFailsOutsideProject ["clean"],
      test "succeed-uncompiled-project" $
        withNewWaspProject minimalStarterTemplate $ do
          waspCleanDeletesBothDirs
          assertDirectoryDoesNotExist dotWaspDir
          assertDirectoryDoesNotExist nodeModulesDir,
      test "succeed-compiled-project" $
        withNewWaspProject minimalStarterTemplate $ do
          wasp ["compile"]
          waspCleanDeletesBothDirs
          assertDirectoryDoesNotExist dotWaspDir
          assertDirectoryDoesNotExist nodeModulesDir
    ]
  where
    -- Checks that Wasp deleted both dirs, instead of finding nothing to
    -- delete, so the does-not-exist checks can't pass just because the dirs
    -- were never there.
    waspCleanDeletesBothDirs :: TestM ()
    waspCleanDeletesBothDirs = do
      result <- wasp ["clean"]
      assertOutputContains "Deleted the node_modules/ directory." result
      assertOutputContains "Deleted the .wasp/ directory." result

    -- Not a literal, since a check that a dir doesn't exist would still pass
    -- if Wasp renamed it.
    dotWaspDir = SP.fromRelDir dotWaspDirInWaspProjectDir
