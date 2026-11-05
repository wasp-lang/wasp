module Tests.WaspNewTest (waspNewTest) where

import ShellCommands
  ( assertCommandOutputContainsAll,
    assertCommandOutputDoesNotContain,
    waspCliNew,
    waspCliNewWithPipedAnswersFails,
    withUnreachableNpmRegistry,
  )
import Test (Test (..), TestCase (..))
import Wasp.Cli.Command.CreateNewProject.AvailableTemplates (basicStarterTemplate, minimalStarterTemplate)
import Wasp.Util.Terminal (styleCode)

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
        (sequence [waspCliNewWithPipedAnswersFails "wasp-app" basicStarterTemplate]),
      TestCase
        "hides-npm-output"
        ( sequence
            [ assertCommandOutputDoesNotContain
                (waspCliNew "wasp-app" minimalStarterTemplate)
                -- npm's install summary, e.g. "added 357 packages, and audited 359 packages in 7s".
                " packages"
            ]
        ),
      TestCase
        "warns-when-install-fails"
        ( sequence
            [ assertCommandOutputContainsAll
                (withUnreachableNpmRegistry $ waspCliNew "wasp-app" minimalStarterTemplate)
                [ "Dependencies failed to install",
                  "Run " ++ styleCode "wasp install"
                ],
              -- The project was created, so npm's output is not dumped on the user.
              assertCommandOutputDoesNotContain
                (withUnreachableNpmRegistry $ waspCliNew "wasp-app-2" minimalStarterTemplate)
                "npm error"
            ]
        )
        -- FIXME(Franjo): These will fail because there "dev" opensaas template tag doesn't exist.
        -- We can't really test the `saas` template in `waspc` e2e tests.
        --   TestCase
        --   "create-saas"
        --   (sequence [waspCliNew "wasp-app" SaaS]),
        -- TestCase
        --   "create-saas-interactive"
        --   (sequence [waspCliNewWithPipedAnswersFails "wasp-app" SaaS])
    ]
