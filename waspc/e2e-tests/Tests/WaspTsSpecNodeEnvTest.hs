{-# LANGUAGE QuasiQuotes #-}

module Tests.WaspTsSpecNodeEnvTest (waspTsSpecNodeEnvTest) where

import qualified Data.Text as T
import NeatInterpolation (trimming)
import ShellCommands
  ( assertCommandOutputContains,
    createTestWaspProject,
    inTestWaspProjectDir,
    replaceMainWaspTsFile,
    setWaspDbToPSQL,
    waspCliBuild,
    waspCliCompile,
  )
import Test (Test (..), TestCase (..))
import Wasp.Cli.Command.CreateNewProject.AvailableTemplates (minimalStarterTemplate)
import Wasp.Version (waspVersion)

waspTsSpecNodeEnvTest :: Test
waspTsSpecNodeEnvTest =
  Test
    "wasp-ts-spec-node-env"
    [ TestCase
        "node-env-is-development-on-compile"
        ( sequence
            [ createTestWaspProject minimalStarterTemplate,
              inTestWaspProjectDir
                [ replaceMainWaspTsFile nodeEnvMainWaspTs,
                  assertCommandOutputContains waspCliCompile "E2E-NODE-ENV=development"
                ]
            ]
        ),
      TestCase
        "node-env-is-production-on-build"
        ( sequence
            [ createTestWaspProject minimalStarterTemplate,
              inTestWaspProjectDir
                [ setWaspDbToPSQL,
                  replaceMainWaspTsFile nodeEnvMainWaspTs,
                  assertCommandOutputContains waspCliBuild "E2E-NODE-ENV=production"
                ]
            ]
        )
    ]
  where
    nodeEnvMainWaspTs :: T.Text
    nodeEnvMainWaspTs =
      [trimming|
        import { app, page, route } from "@wasp.sh/spec";
        import { MainPage } from "./src/MainPage" with { type: "ref" }

        console.log(`E2E-NODE-ENV=$${process.env.NODE_ENV}`);

        export default app({
          name: "tsSpecNodeEnvTest",
          title: "tsSpecNodeEnvTest",
          wasp: { version: "$textWaspVersion" },
          head: ["<link rel='icon' href='/favicon.ico' />"],
          parts: [
            route("RootRoute", "/", page(MainPage)),
          ]
        })
      |]

    textWaspVersion :: T.Text
    textWaspVersion = T.pack . show $ waspVersion
