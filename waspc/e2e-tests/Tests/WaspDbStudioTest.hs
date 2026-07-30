module Tests.WaspDbStudioTest (waspDbStudioTest) where

import Command (Command)
import SharedActions (createWaspProject, inWaspProjectDir, runCommand, runCommandExpectingFailure, waspCli)
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

waspCliDbStudio :: Command
waspCliDbStudio = waspCli ["db", "studio"]
