module Tests.WaspNewTest (waspNewTest) where

import qualified Data.Text as T
import E2e
import Wasp.Cli.Command.CreateNewProject.AvailableTemplates (basicStarterTemplate, minimalStarterTemplate)
import Wasp.Cli.Command.CreateNewProject.StarterTemplates (StarterTemplate)

waspNewTest :: E2eTest
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
    -- Without an interactive terminal, Wasp refuses to prompt for the answers.
    createWaspProjectWithPipedAnswersFails :: StarterTemplate -> TestM ()
    createWaspProjectWithPipedAnswersFails template =
      runExpectingFailure (withStdinText (T.pack $ waspAppDirName ++ "\n" ++ show template ++ "\n") $ waspCli ["new"])
        >>= assertOutputContains "stdin is not an interactive terminal"
