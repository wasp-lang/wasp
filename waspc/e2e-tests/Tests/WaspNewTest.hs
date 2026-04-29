module Tests.WaspNewTest (waspNewTest) where

import ShellCommands (waspCliNew, waspCliNewInteractive)
import Test (Test (..), TestCase (..))
import Wasp.Cli.Command.CreateNewProject.AvailableTemplates (basicStarterTemplate, minimalStarterTemplate)

-- TODO: add "new ai" tests
waspNewTest :: Test
waspNewTest =
  Test
    "wasp-new"
    [ TestCase
        "create-minimal"
        (sequence [waspCliNew "wasp-app" minimalStarterTemplate]),
      TestCase
        "create-minimal-interactive"
        (sequence [waspCliNewInteractive "wasp-app" minimalStarterTemplate]),
      TestCase
        "create-basic"
        (sequence [waspCliNew "wasp-app" basicStarterTemplate]),
      TestCase
        "create-basic-interactive"
        (sequence [waspCliNewInteractive "wasp-app" basicStarterTemplate])
        -- FIXME(Franjo): These will fail because there "dev" opensaas template tag doesn't exist.
        -- We can't really test the `saas` template in `waspc` e2e tests.
        --   TestCase
        --   "create-saas"
        --   (sequence [waspCliNew "wasp-app" SaaS]),
        -- TestCase
        --   "create-saas-interactive"
        --   (sequence [waspCliNewInteractive "wasp-app" SaaS])
    ]
