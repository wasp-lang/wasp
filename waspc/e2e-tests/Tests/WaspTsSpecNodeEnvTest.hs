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
import Wasp.Cli.Command.CreateNewProject.AvailableTemplates (tsMinimalStarterTemplate)
import Wasp.Version (waspVersion)

waspTsSpecNodeEnvTest :: Test
waspTsSpecNodeEnvTest =
  Test
    "wasp-ts-spec-node-env"
    [ TestCase
        "node-env-is-development-on-compile"
        ( sequence
            [ createTestWaspProject tsMinimalStarterTemplate,
              inTestWaspProjectDir
                [ replaceMainWaspTsFile nodeEnvMainWaspTs,
                  assertCommandOutputContains waspCliCompile "E2E-NODE-ENV=development"
                ]
            ]
        ),
      TestCase
        "node-env-is-production-on-build"
        ( sequence
            [ createTestWaspProject tsMinimalStarterTemplate,
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
        import { App } from "wasp-config";

        console.log(`E2E-NODE-ENV=$${process.env.NODE_ENV}`);

        const app = new App("envprobe", {
          title: "envprobe",
          wasp: { version: "^$textWaspVersion" },
        });

        const mainPage = app.page("MainPage", {
          component: { import: "MainPage", from: "@src/MainPage" },
        });

        app.route("RootRoute", { path: "/", to: mainPage });

        export default app;
      |]

    textWaspVersion :: T.Text
    textWaspVersion = T.pack . show $ waspVersion
