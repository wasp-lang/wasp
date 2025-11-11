{-
NOTE: We don't test the @ai@ template in @waspc@ e2e tests.
-}
module EphemeralTest.WaspNewEphemeralTest (waspNewMinimalEphemeralTest, waspNewInteractiveMinimalEphemeralTest, waspNewBasicEphemeralTest, waspNewInteractiveBasicEphemeralTest, waspNewSaasEphemeralTest, waspNewInteractiveSaasEphemeralTest) where

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

waspNewInteractiveMinimalEphemeralTest :: EphemeralTest
waspNewInteractiveMinimalEphemeralTest =
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

waspNewInteractiveBasicEphemeralTest :: EphemeralTest
waspNewInteractiveBasicEphemeralTest =
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

waspNewInteractiveSaasEphemeralTest :: EphemeralTest
waspNewInteractiveSaasEphemeralTest =
  makeEphemeralTest
    "wasp-new-saas-interactive"
    [ makeEphemeralTestCase
        "Should create a saas template project interactively succesfully"
        (waspCliNewInteractive "wasp-app" SaaS)
    ]
