{-# LANGUAGE QuasiQuotes #-}

module Tests.WaspTsSpecNodeEnvTest (waspTsSpecNodeEnvTest) where

import qualified Data.Text as T
import NeatInterpolation (trimming)
import ShellCommands
  ( ShellCommand,
    ShellCommandBuilder,
    WaspProjectContext,
    createTestWaspProject,
    inTestWaspProjectDir,
    replaceMainWaspTsFile,
    setWaspDbToPSQL,
    waspCliBuild,
    waspCliCompile,
    waspCliTsSetup,
    (~&&),
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
                [ waspCliTsSetup,
                  replaceMainWaspTsFile nodeEnvMainWaspTs,
                  assertCommandOutputContains waspCliCompile "E2E-NODE-ENV=development"
                ]
            ]
        ),
      TestCase
        "node-env-is-production-on-build"
        ( sequence
            [ createTestWaspProject tsMinimalStarterTemplate,
              inTestWaspProjectDir
                [ waspCliTsSetup,
                  setWaspDbToPSQL,
                  replaceMainWaspTsFile nodeEnvMainWaspTs,
                  assertCommandOutputContains waspCliBuild "E2E-NODE-ENV=production"
                ]
            ]
        )
    ]
  where
    assertCommandOutputContains ::
      ShellCommandBuilder WaspProjectContext ShellCommand ->
      String ->
      ShellCommandBuilder WaspProjectContext ShellCommand
    assertCommandOutputContains commandBuilder marker = do
      command <- commandBuilder
      let logFile = "main-wasp-ts.log"
          logCommandOutputToFile = command ++ " > " ++ logFile ++ " 2>&1"
          searchMarkerInLogFile = "grep -qF '" ++ marker ++ "' " ++ logFile

      return $ logCommandOutputToFile ~&& searchMarkerInLogFile

    nodeEnvMainWaspTs :: T.Text
    nodeEnvMainWaspTs =
      [trimming|
        import { app, page, route } from "wasp-config";
        // @ts-ignore
        import { MainPage } from "@src/MainPage"

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
