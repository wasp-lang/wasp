{-# LANGUAGE QuasiQuotes #-}

module Tests.WaspShowTest (waspShowTest) where

import qualified Data.Text as T
import NeatInterpolation (trimming)
import ShellCommands (ShellCommand, createTestWaspProject, inTestWaspProjectDir, replaceMainWaspTsFile, waspCliShowBuild, waspCliShowBuildJson, waspCliShowSpec, waspCliShowSpecJson, (~&&))
import Test (Test (..), TestCase (..))
import Wasp.Cli.Command.CreateNewProject.AvailableTemplates (minimalStarterTemplate)

waspShowTest :: Test
waspShowTest =
  Test
    "wasp-show"
    [ TestCase
        "fail-without-subcommand"
        (return [waspCliShowFails]),
      TestCase
        "fail-outside-project"
        (return [waspCliShowSpecFails, waspCliShowBuildFails]),
      TestCase
        "succeed-inside-project"
        ( sequence
            [ createTestWaspProject minimalStarterTemplate,
              inTestWaspProjectDir
                [ waspCliShowSpec,
                  waspCliShowSpecJson,
                  waspCliShowBuild,
                  waspCliShowBuildJson
                ]
            ]
        ),
      TestCase
        "concurrent-spec-analyses-read-their-own-results"
        -- `wasp show spec` doesn't lock the project, so its analysis can overlap
        -- with others (e.g. a `wasp start` recompile). Each analysis here
        -- declares a different route (taken from an env var), and must only see
        -- its own.
        ( sequence
            [ createTestWaspProject minimalStarterTemplate,
              inTestWaspProjectDir
                [ replaceMainWaspTsFile routeFromEnvVarMainWaspTs,
                  return runConcurrentSpecAnalyses,
                  return assertEachSpecAnalysisSawItsOwnRoute
                ]
            ]
        )
    ]
  where
    concurrentSpecAnalysesIndices :: String
    concurrentSpecAnalysesIndices = "1 2 3 4 5 6 7 8"

    runConcurrentSpecAnalyses :: ShellCommand
    runConcurrentSpecAnalyses =
      "WASP_E2E_SPEC_PIDS=''"
        ~&& ( "for i in "
                ++ concurrentSpecAnalysesIndices
                ++ "; do"
                ++ " { E2E_ROUTE=\"e2e-route-$i\" $WASP_CLI_CMD show spec --json > \".wasp-e2e-spec-$i.json\" 2>&1 & };"
                ++ " WASP_E2E_SPEC_PIDS=\"$WASP_E2E_SPEC_PIDS $!\";"
                ++ " done"
            )
        ~&& "for pid in $WASP_E2E_SPEC_PIDS; do wait \"$pid\" || exit 1; done"

    assertEachSpecAnalysisSawItsOwnRoute :: ShellCommand
    assertEachSpecAnalysisSawItsOwnRoute =
      "for i in "
        ++ concurrentSpecAnalysesIndices
        ++ "; do grep -qF \"/e2e-route-$i\\\"\" \".wasp-e2e-spec-$i.json\" || exit 1; done"

    routeFromEnvVarMainWaspTs :: T.Text
    routeFromEnvVarMainWaspTs =
      [trimming|
        import { app, page, route } from "@wasp/spec";
        import { MainPage } from "./src/MainPage" with { type: "ref" }

        export default app({
          name: "concurrentSpecAnalysesTest",
          title: "concurrentSpecAnalysesTest",
          head: ["<link rel='icon' href='/favicon.ico' />"],
          spec: [
            route("RootRoute", `/$${process.env.E2E_ROUTE}`, page(MainPage)),
          ]
        })
      |]

    waspCliShowFails :: ShellCommand
    waspCliShowFails = "! $WASP_CLI_CMD show"

    waspCliShowSpecFails :: ShellCommand
    waspCliShowSpecFails = "! $WASP_CLI_CMD show spec"

    waspCliShowBuildFails :: ShellCommand
    waspCliShowBuildFails = "! $WASP_CLI_CMD show build"
