module Tests.WaspNewTest (waspNewTest) where

import ShellCommands (WaspNewTemplate (..), waspCliNew, waspCliNewInteractive)
import Test (Test (..), TestCase (..))

-- TODO: add "new ai" tests
waspNewTest :: Test
waspNewTest =
  Test
    "wasp-new"
    [ TestCase
        "create-minimal"
        (sequence [waspCliNew "wasp-app" Minimal]),
      TestCase
        "create-minimal-interactive"
        (sequence [waspCliNewInteractive "wasp-app" Minimal]),
      TestCase
        "create-basic"
        (sequence [waspCliNew "wasp-app" Basic]),
      TestCase
        "create-basic-interactive"
        (sequence [waspCliNewInteractive "wasp-app" Basic])
        -- FIXME(Franjo): These will fail because there "dev" opensaas template tag doesn't exist.
        -- We can't really test the `saas` template in `waspc` e2e tests.
        --   TestCase
        --   "create-saas"
        --   (sequence [waspCliNew "wasp-app" SaaS]),
        -- TestCase
        --   "create-saas-interactive"
        --   (sequence [waspCliNewInteractive "wasp-app" SaaS])
    ]
