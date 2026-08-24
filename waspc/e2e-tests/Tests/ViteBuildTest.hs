module Tests.ViteBuildTest (viteBuildTest) where

import Control.Monad.Reader (ask)
import qualified Data.Text as T
import NeatInterpolation (trimming)
import ShellCommands
  ( ShellCommand,
    ShellCommandBuilder,
    TestContext,
    WaspProjectContext (..),
    appendToFile,
    createTestWaspProject,
    inTestWaspProjectDir,
    replaceMainWaspTsFile,
    setWaspDbToPSQL,
    waspCliBuild,
    writeToFile,
  )
import StrongPath (relfile, (</>))
import qualified StrongPath as SP
import Test (Test (..), TestCase (..))
import Wasp.Cli.Command.CreateNewProject.AvailableTemplates (minimalStarterTemplate)
import Wasp.Generator.WebAppGenerator (viteBuildDirPath)
import Wasp.Project.Env (dotEnvClient)

viteBuildTest :: Test
viteBuildTest =
  Test
    "vite-build"
    [ TestCase
        "fail-on-missing-required-env-vars"
        (createViteBuildTestCase [expectCommandFailure <$> viteBuild]),
      TestCase
        "success-with-required-env-vars"
        (createViteBuildTestCase [appendInlineEnvVars [apiUrlEnvVar] <$> viteBuild]),
      TestCase
        "fail-missing-inline-env-var"
        ( createViteBuildTestCase
            [ appendInlineEnvVars [apiUrlEnvVar] <$> viteBuild,
              expectCommandFailure <$> assertBuildOutputContains inlineEnvVarValue
            ]
        ),
      TestCase
        -- Based on https://github.com/wasp-lang/wasp/issues/3741
        "succeed-inline-env-var"
        ( createViteBuildTestCase
            [ appendInlineEnvVars [apiUrlEnvVar, (testEnvVarKey, inlineEnvVarValue)] <$> viteBuild,
              assertBuildOutputContains inlineEnvVarValue
            ]
        ),
      TestCase
        "ignore-dotenv-client-file-in-build"
        ( createViteBuildTestCase
            [ writeDotEnvClientFile dotEnvFileValue,
              appendInlineEnvVars [apiUrlEnvVar] <$> viteBuild,
              expectCommandFailure <$> assertBuildOutputContains dotEnvFileValue
            ]
        ),
      TestCase
        "inline-env-vars-work-with-env-file-present"
        ( createViteBuildTestCase
            [ writeDotEnvClientFile dotEnvFileValue,
              appendInlineEnvVars [apiUrlEnvVar, (testEnvVarKey, inlineEnvVarValue)] <$> viteBuild,
              assertBuildOutputContains inlineEnvVarValue
            ]
        ),
      TestCase
        "fail-on-missing-custom-client-env-var"
        ( createClientEnvSchemaViteBuildTestCase
            [expectCommandFailure <$> viteBuildWithApiUrl]
        ),
      TestCase
        "success-with-custom-client-env-var"
        ( createClientEnvSchemaViteBuildTestCase
            [appendInlineEnvVars [apiUrlEnvVar, customEnvVar] <$> viteBuild]
        ),
      TestCase
        "fail-on-user-code-type-error"
        ( createViteBuildTestCase
            [ addTypeErrorToSrcFile,
              expectCommandFailure <$> viteBuildWithApiUrl
            ]
        ),
      TestCase
        "ignore-wasp-ts-type-errors"
        ( createViteBuildTestCase
            [ addTypeErrorToWaspTsFile,
              viteBuildWithApiUrl
            ]
        ),
      TestCase
        "fail-on-user-defined-client-port"
        ( createViteBuildTestCase
            [ writeViteConfigWithServerPort,
              expectCommandFailure <$> viteBuildWithApiUrl
            ]
        )
    ]
  where
    createViteBuildTestCase :: [ShellCommandBuilder WaspProjectContext ShellCommand] -> ShellCommandBuilder TestContext [ShellCommand]
    createViteBuildTestCase commands =
      sequence
        [ createTestWaspProject minimalStarterTemplate,
          inTestWaspProjectDir $ [setWaspDbToPSQL, writeMainPageTsx, waspCliBuild] ++ commands
        ]

    createClientEnvSchemaViteBuildTestCase :: [ShellCommandBuilder WaspProjectContext ShellCommand] -> ShellCommandBuilder TestContext [ShellCommand]
    createClientEnvSchemaViteBuildTestCase commands =
      sequence
        [ createTestWaspProject minimalStarterTemplate,
          inTestWaspProjectDir $
            [ setWaspDbToPSQL,
              writeClientEnvSchema,
              writeClientEnvMainPageTsx,
              replaceMainWaspTsFile mainWaspTsWithClientEnvSchema,
              waspCliBuild
            ]
              ++ commands
        ]

    viteBuild :: ShellCommandBuilder WaspProjectContext ShellCommand
    viteBuild = return "npx vite build"

    viteBuildWithApiUrl :: ShellCommandBuilder WaspProjectContext ShellCommand
    viteBuildWithApiUrl = appendInlineEnvVars [apiUrlEnvVar] <$> viteBuild

    assertBuildOutputContains :: String -> ShellCommandBuilder WaspProjectContext ShellCommand
    assertBuildOutputContains value = return $ "grep -r '" ++ value ++ "' " ++ SP.fromRelDir viteBuildDirPath

    writeMainPageTsx :: ShellCommandBuilder WaspProjectContext ShellCommand
    writeMainPageTsx = do
      waspProjectContext <- ask
      let testEnvVarKeyText = T.pack testEnvVarKey
      writeToFile
        (waspProjectContext.waspProjectDir </> [relfile|src/MainPage.tsx|])
        [trimming|
          export function MainPage() {
            return <h2>{import.meta.env.${testEnvVarKeyText}}</h2>
          }
        |]

    writeClientEnvSchema :: ShellCommandBuilder WaspProjectContext ShellCommand
    writeClientEnvSchema = do
      waspProjectContext <- ask
      writeToFile
        (waspProjectContext.waspProjectDir </> [relfile|src/env.ts|])
        [trimming|
          import { defineEnvValidationSchema } from "wasp/env";
          import * as z from "zod";

          export const clientEnvValidationSchema = defineEnvValidationSchema(z.object({
            REACT_APP_CUSTOM: z.string(),
          }));
        |]

    writeClientEnvMainPageTsx :: ShellCommandBuilder WaspProjectContext ShellCommand
    writeClientEnvMainPageTsx = do
      waspProjectContext <- ask
      writeToFile
        (waspProjectContext.waspProjectDir </> [relfile|src/MainPage.tsx|])
        [trimming|
          import { env } from "wasp/client";

          export function MainPage() {
            return <h2>{env.REACT_APP_CUSTOM}</h2>;
          }
        |]

    mainWaspTsWithClientEnvSchema =
      [trimming|
        import { app, page, route } from "@wasp.sh/spec";
        import { MainPage } from "./src/MainPage" with { type: "ref" };
        import { clientEnvValidationSchema } from "./src/env" with { type: "ref" };

        export default app({
          name: "ClientEnvSchemaTest",
          wasp: { version: "0.26.0" },
          title: "Client Env Schema Test",
          client: { envValidationSchema: clientEnvValidationSchema },
          spec: [route("RootRoute", "/", page(MainPage))],
        });
      |]

    writeViteConfigWithServerPort :: ShellCommandBuilder WaspProjectContext ShellCommand
    writeViteConfigWithServerPort = do
      waspProjectContext <- ask
      writeToFile
        (waspProjectContext.waspProjectDir </> [relfile|vite.config.ts|])
        [trimming|
          import { wasp } from "wasp/client/vite";
          import { defineConfig } from "vite";

          export default defineConfig({
            plugins: [wasp()],
            server: {
              port: 4000,
            },
          });
        |]

    writeDotEnvClientFile :: String -> ShellCommandBuilder WaspProjectContext ShellCommand
    writeDotEnvClientFile value = do
      waspProjectContext <- ask
      writeToFile (waspProjectContext.waspProjectDir </> dotEnvClient) $
        T.pack $
          testEnvVarKey ++ "=" ++ value

    addTypeErrorToSrcFile :: ShellCommandBuilder WaspProjectContext ShellCommand
    addTypeErrorToSrcFile = appendToFile "src/MainPage.tsx" typeError

    addTypeErrorToWaspTsFile :: ShellCommandBuilder WaspProjectContext ShellCommand
    addTypeErrorToWaspTsFile = appendToFile "main.wasp.ts" typeError

    typeError :: T.Text
    typeError = "const shouldBeString: string = 123"

    appendInlineEnvVars :: [(String, String)] -> ShellCommand -> ShellCommand
    appendInlineEnvVars envVars command = foldr appendInlineEnvVar command envVars

    appendInlineEnvVar :: (String, String) -> ShellCommand -> ShellCommand
    appendInlineEnvVar (key, value) command = key ++ "=" ++ value ++ " " ++ command

    apiUrlEnvVar :: (String, String)
    apiUrlEnvVar = ("REACT_APP_API_URL", "http://localhost:3001")

    customEnvVar :: (String, String)
    customEnvVar = ("REACT_APP_CUSTOM", "CustomValue")

    testEnvVarKey :: String
    testEnvVarKey = "REACT_APP_NAME"

    inlineEnvVarValue :: String
    inlineEnvVarValue = "RandomNameTest"

    dotEnvFileValue :: String
    dotEnvFileValue = "DotEnvFileValue"

    expectCommandFailure :: ShellCommand -> ShellCommand
    expectCommandFailure command = "! " ++ command
