module Tests.WaspNewTest (waspNewTest) where

import qualified Data.Text as T
import E2E
import Wasp.Cli.Command.CreateNewProject.AvailableTemplates (basicStarterTemplate, minimalStarterTemplate)
import Wasp.Cli.Command.CreateNewProject.StarterTemplates (StarterTemplate)

waspNewTest :: E2ETest
waspNewTest =
  testGroup
    "wasp-new"
    [ test "create-minimal" $ createWaspProject minimalStarterTemplate,
      test "create-minimal-with-piped-answers-fails" $ createWaspProjectWithPipedAnswersFails minimalStarterTemplate,
      test "create-basic" $ createWaspProject basicStarterTemplate,
      test "create-basic-with-piped-answers-fails" $ createWaspProjectWithPipedAnswersFails basicStarterTemplate
      -- NOTE: We can't test the `saas` template here, since its "dev" tag doesn't exist.
    ]
  where
    createWaspProjectWithPipedAnswersFails :: StarterTemplate -> TestM ()
    createWaspProjectWithPipedAnswersFails template =
      assertFailsWithoutInteractiveTerminal "wasp new"
        $ withStdinText (T.pack $ waspAppDirName ++ "\n" ++ show template ++ "\n")
        $ waspCli ["new"]
