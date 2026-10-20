module Tests.WaspNewTest (waspNewTest) where

import ShellCommands (waspCliNew, waspCliNewWithPipedAnswersFails)
import Test (Test (..), TestCase (..))
import Wasp.Cli.Command.CreateNewProject.AvailableTemplates (basicStarterTemplate, minimalStarterTemplate)

waspNewTest :: Test
waspNewTest =
  Test
    "wasp-new"
    [ TestCase
        "create-minimal"
        (sequence [waspCliNew "wasp-app" minimalStarterTemplate]),
      TestCase
        "create-minimal-with-piped-answers-fails"
        (sequence [waspCliNewWithPipedAnswersFails "wasp-app" minimalStarterTemplate]),
      TestCase
        "create-basic"
        (sequence [waspCliNew "wasp-app" basicStarterTemplate]),
      TestCase
        "create-basic-with-piped-answers-fails"
        (sequence [waspCliNewWithPipedAnswersFails "wasp-app" basicStarterTemplate])
        -- FIXME(Franjo): These will fail because there "dev" opensaas template tag doesn't exist.
        -- We can't really test the `saas` template in `waspc` e2e tests.
        --   TestCase
        --   "create-saas"
        --   (sequence [waspCliNew "wasp-app" SaaS]),
        -- TestCase
        --   "create-saas-interactive"
        --   (sequence [waspCliNewWithPipedAnswersFails "wasp-app" SaaS])
    ]
