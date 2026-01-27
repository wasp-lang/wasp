module Tests.WaspNewTest (waspNewTest) where

import ShellCommands (WaspNewTemplate (..), waspCliNew, waspCliNewInteractive)
import Test (Test, makeTest, makeTestCase)

-- TODO: add "new ai" tests
waspNewTest :: Test
waspNewTest =
  makeTest
    "wasp-new"
    [ makeTestCase
        "create-minimal"
        (sequence [waspCliNew "wasp-app" Minimal]),
      makeTestCase
        "create-minimal-interactive"
        (sequence [waspCliNewInteractive "wasp-app" Minimal]),
      makeTestCase
        "create-basic"
        (sequence [waspCliNew "wasp-app" Basic]),
      makeTestCase
        "create-basic-interactive"
        (sequence [waspCliNewInteractive "wasp-app" Basic])
        -- FIXME(Franjo): These will fail because there "dev" opensaas template tag doesn't exist.
        -- We can't really test the `saas` template in `waspc` e2e tests.
        --   makeTestCase
        --   "create-saas"
        --   (sequence [waspCliNew "wasp-app" SaaS]),
        -- makeTestCase
        --   "create-saas-interactive"
        --   (sequence [waspCliNewInteractive "wasp-app" SaaS])
    ]
