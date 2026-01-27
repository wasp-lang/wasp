{-
NOTE: We don't test the @ai@ template in @waspc@ e2e tests.
-}
module Test.WaspNewTest (waspNewMinimalTest, waspNewMinimalInteractiveTest, waspNewBasicTest, waspNewBasicInteractiveTest, waspNewSaasTest, waspNewSaasInteractiveTest) where

import ShellCommands (WaspNewTemplate (..), waspCliNew, waspCliNewInteractive)
import Test (Test, makeTest, makeTestCase)

waspNewMinimalTest :: Test
waspNewMinimalTest =
  makeTest
    "wasp-new-minimal"
    [ makeTestCase
        "Should create a minimal template project succesfully"
        (sequence [waspCliNew "wasp-app" Minimal])
    ]

waspNewMinimalInteractiveTest :: Test
waspNewMinimalInteractiveTest =
  makeTest
    "wasp-new-minimal-interactive"
    [ makeTestCase
        "Should create a minimal template project interactively succesfully"
        (sequence [waspCliNewInteractive "wasp-app" Minimal])
    ]

waspNewBasicTest :: Test
waspNewBasicTest =
  makeTest
    "wasp-new-basic"
    [ makeTestCase
        "Should create a basic template project succesfully"
        (sequence [waspCliNew "wasp-app" Basic])
    ]

waspNewBasicInteractiveTest :: Test
waspNewBasicInteractiveTest =
  makeTest
    "wasp-new-basic-interactive"
    [ makeTestCase
        "Should create a basic template project interactively succesfully"
        (sequence [waspCliNewInteractive "wasp-app" Basic])
    ]

waspNewSaasTest :: Test
waspNewSaasTest =
  makeTest
    "wasp-new-saas"
    [ makeTestCase
        "Should create a saas template project succesfully"
        (sequence [waspCliNew "wasp-app" SaaS])
    ]

waspNewSaasInteractiveTest :: Test
waspNewSaasInteractiveTest =
  makeTest
    "wasp-new-saas-interactive"
    [ makeTestCase
        "Should create a saas template project interactively succesfully"
        (sequence [waspCliNewInteractive "wasp-app" SaaS])
    ]
