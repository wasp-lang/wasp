module Tests.WaspDbStudioTest (waspDbStudioTest) where

import Steps (createWaspProject, inWaspProjectDir, runCommand, runCommandExpectingFailure, waspCliDbStudio)
import Test (Test (..), TestCase (..))
import Wasp.Cli.Command.CreateNewProject.AvailableTemplates (minimalStarterTemplate)

-- | NOTE: We don't test feature content since it's prisma feature.
-- FIXME: @waspCliDbStudio@ - figure out long lasting processes
waspDbStudioTest :: Test
waspDbStudioTest =
  Test
    "wasp-db-studio"
    [ TestCase "fail-outside-project" $
        runCommandExpectingFailure waspCliDbStudio,
      TestCase "succeed-uncompiled-project" $ do
        createWaspProject minimalStarterTemplate
        inWaspProjectDir $
          runCommand waspCliDbStudio
    ]
