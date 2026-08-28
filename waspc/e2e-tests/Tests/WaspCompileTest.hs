module Tests.WaspCompileTest (waspCompileTest) where

import Control.Monad.Reader (ask)
import NeatInterpolation (trimming)
import ShellCommands
  ( ShellCommand,
    WaspProjectContext (..),
    createTestWaspProject,
    inTestWaspProjectDir,
    replaceMainWaspTsFile,
    setWaspDbToPSQL,
    waspCliCompile,
    writeToFile,
  )
import StrongPath (relfile, (</>))
import Test (Test (..), TestCase (..))
import Wasp.Cli.Command.CreateNewProject.AvailableTemplates (minimalStarterTemplate)

waspCompileTest :: Test
waspCompileTest =
  Test
    "wasp-compile"
    [ TestCase
        "fail-outside-project"
        (return [waspCliCompileFails]),
      TestCase
        "succeed-uncompiled-project"
        ( sequence
            [ createTestWaspProject minimalStarterTemplate,
              inTestWaspProjectDir
                [ waspCliCompile,
                  return $ assertDirectoryExists ".wasp",
                  return $ assertDirectoryExists "node_modules"
                ]
            ]
        ),
      TestCase
        "succeed-compiled-project"
        ( sequence
            [ createTestWaspProject minimalStarterTemplate,
              inTestWaspProjectDir
                [ waspCliCompile,
                  waspCliCompile,
                  return $ assertDirectoryExists ".wasp",
                  return $ assertDirectoryExists "node_modules"
                ]
            ]
        ),
      TestCase
        "generates-and-bundles-runtime-bindings"
        ( sequence
            [ createTestWaspProject minimalStarterTemplate,
              inTestWaspProjectDir
                [ writeEnvSchemas,
                  writePrismaSetup,
                  writeOperation,
                  replaceMainWaspTsFile mainWaspTsWithRuntimeBindings,
                  setWaspDbToPSQL,
                  waspCliCompile,
                  return $ assertFileContains "../../../../src/env" "./.wasp/out/server/src/runtimeBindings.ts",
                  return $ assertFileContains "../../../../src/prisma" "./.wasp/out/server/src/runtimeBindings.ts",
                  return $ assertFileContains "../../../../src/operation" "./.wasp/out/server/src/runtimeBindings.ts",
                  return $ assertFileContains "() => import" "./.wasp/out/server/src/runtimeBindings.ts",
                  return $ assertFileContains "operations" "./.wasp/out/server/src/runtimeBindings.ts",
                  return $ assertFileContains "getServerOperation" "./.wasp/out/sdk/wasp/server/operations/queries/index.ts",
                  return $ assertFileDoesNotContain "virtual:wasp/user" "./.wasp/out/sdk/wasp/server/operations/queries/index.ts",
                  return $ assertFileDoesNotContain "virtual:wasp/user" "./.wasp/out/sdk/wasp/server/dbClient.ts",
                  return $ assertFileContains "./src/env" "./.wasp/out/sdk/wasp/client/vite/virtual-files/files/client-runtime-bindings.ts",
                  return $ assertFileDoesNotContain "virtual:wasp/" "./.wasp/out/sdk/wasp/client/env/schema.ts",
                  return $ assertFileDoesNotContain "virtual:wasp/user" "./.wasp/out/sdk/wasp/client/env/schema.ts",
                  return $ assertFileContains "\"wasp\": \"file:../sdk/wasp\"" "./.wasp/out/server/package.json",
                  return "(cd .wasp/out/server && npm run build)",
                  return $ assertFileContains "await import" "./.wasp/out/server/build/bootstrap.js",
                  return $ assertFileContains "wasp/server/runtime" "./.wasp/out/server/build/bootstrap.js"
                ]
            ]
        )
    ]
  where
    waspCliCompileFails :: ShellCommand
    waspCliCompileFails = "! $WASP_CLI_CMD compile"

    assertDirectoryExists :: FilePath -> ShellCommand
    assertDirectoryExists dirFilePath = "[ -d '" ++ dirFilePath ++ "' ]"

    writeEnvSchemas = do
      context <- ask
      writeToFile
        (context.waspProjectDir </> [relfile|src/env.ts|])
        [trimming|
          import { defineEnvValidationSchema } from "wasp/env";
          import * as z from "zod";

          export const serverEnvValidationSchema = defineEnvValidationSchema(z.object({}));
          export const clientEnvValidationSchema = defineEnvValidationSchema(z.object({
            REACT_APP_NAME: z.string(),
          }));
        |]

    writePrismaSetup = do
      context <- ask
      writeToFile
        (context.waspProjectDir </> [relfile|src/prisma.ts|])
        [trimming|
          import { PrismaClient } from "@prisma/client";

          export const setUpPrisma = () => new PrismaClient();
        |]

    writeOperation = do
      context <- ask
      writeToFile
        (context.waspProjectDir </> [relfile|src/operation.ts|])
        [trimming|
          import { getOtherValue as getOtherValueApi } from "wasp/server/operations";

          export const getValue = async () => getOtherValueApi();
          export const getOtherValue = async () => "value";
        |]

    mainWaspTsWithRuntimeBindings =
      [trimming|
        import { app, page, query, route } from "@wasp.sh/spec";
        import { MainPage } from "./src/MainPage" with { type: "ref" };
        import { clientEnvValidationSchema, serverEnvValidationSchema } from "./src/env" with { type: "ref" };
        import { getOtherValue, getValue } from "./src/operation" with { type: "ref" };
        import { setUpPrisma } from "./src/prisma" with { type: "ref" };

        export default app({
          name: "EnvSchemaTest",
          wasp: { version: "0.26.0" },
          title: "Env Schema Test",
           client: { envValidationSchema: clientEnvValidationSchema },
           server: { envValidationSchema: serverEnvValidationSchema },
           db: { prismaSetupFn: setUpPrisma },
         spec: [
           route("RootRoute", "/", page(MainPage)),
           query(getValue, {}),
           query(getOtherValue, {}),
         ],
         });
      |]

    assertFileContains :: String -> FilePath -> ShellCommand
    assertFileContains marker filePath = "grep -qF '" ++ marker ++ "' " ++ filePath

    assertFileDoesNotContain :: String -> FilePath -> ShellCommand
    assertFileDoesNotContain marker filePath = "! grep -qF '" ++ marker ++ "' " ++ filePath
