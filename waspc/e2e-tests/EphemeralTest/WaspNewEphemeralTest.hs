{-
NOTE: We don't test the @ai@ template in @waspc@ e2e tests.
-}
module EphemeralTest.WaspNewEphemeralTest (waspNewMinimalEphemeralTest, waspNewMinimalInteractiveEphemeralTest, waspNewBasicEphemeralTest, waspNewBasicInteractiveEphemeralTest, waspNewSaasEphemeralTest, waspNewSaasInteractiveEphemeralTest) where

import EphemeralTest (EphemeralTest, makeEphemeralTest, makeEphemeralTestCase)
import ShellCommands (WaspNewTemplate (..), waspCliNew, waspCliNewInteractive)

waspNewMinimalEphemeralTest :: EphemeralTest
waspNewMinimalEphemeralTest =
  makeEphemeralTest
    "wasp-new-minimal"
    [ makeEphemeralTestCase
        "Should create a minimal template project succesfully"
        (waspCliNew "wasp-app" Minimal)
    ]

waspNewMinimalInteractiveEphemeralTest :: EphemeralTest
waspNewMinimalInteractiveEphemeralTest =
  makeEphemeralTest
    "wasp-new-minimal-interactive"
    [ makeEphemeralTestCase
        "Should create a minimal template project interactively succesfully"
        (waspCliNewInteractive "wasp-app" Minimal)
    ]

waspNewBasicEphemeralTest :: EphemeralTest
waspNewBasicEphemeralTest =
  makeEphemeralTest
    "wasp-new-basic"
    [ makeEphemeralTestCase
        "Should create a basic template project succesfully"
        (waspCliNew "wasp-app" Basic)
    ]

waspNewBasicInteractiveEphemeralTest :: EphemeralTest
waspNewBasicInteractiveEphemeralTest =
  makeEphemeralTest
    "wasp-new-basic-interactive"
    [ makeEphemeralTestCase
        "Should create a basic template project interactively succesfully"
        (waspCliNewInteractive "wasp-app" Basic)
    ]

waspNewSaasEphemeralTest :: EphemeralTest
waspNewSaasEphemeralTest =
  makeEphemeralTest
    "wasp-new-saas"
    [ makeEphemeralTestCase
        "Should create a saas template project succesfully"
        (waspCliNew "wasp-app" SaaS)
    ]

waspNewSaasInteractiveEphemeralTest :: EphemeralTest
waspNewSaasInteractiveEphemeralTest =
  makeEphemeralTest
    "wasp-new-saas-interactive"
    [ makeEphemeralTestCase
        "Should create a saas template project interactively succesfully"
        (waspCliNewInteractive "wasp-app" SaaS)
    ]
