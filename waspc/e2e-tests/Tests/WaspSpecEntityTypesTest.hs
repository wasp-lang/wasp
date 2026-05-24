module Tests.WaspSpecEntityTypesTest (waspSpecEntityTypesTest) where

import qualified Data.Text as T
import NeatInterpolation (trimming)
import ShellCommands
  ( appendToPrismaFile,
    createTestWaspProject,
    inTestWaspProjectDir,
    replaceMainWaspTsFile,
    waspCliCompile,
  )
import Test (Test (..), TestCase (..))
import Wasp.Cli.Command.CreateNewProject.AvailableTemplates (minimalStarterTemplate)
import Wasp.Version (waspVersion)

waspSpecEntityTypesTest :: Test
waspSpecEntityTypesTest =
  Test
    "wasp-spec-entity-types"
    [ TestCase
        "TypeScript errors when an invalid Entity name is provided"
        ( sequence
            [ createTestWaspProject minimalStarterTemplate,
              inTestWaspProjectDir
                [ appendToPrismaFile prismaUserModel,
                  replaceMainWaspTsFile mainWaspTsWithError,
                  waspCliCompile, -- Necessary to generate the spec's Entity types.
                  return "npx tsc --noEmit -p tsconfig.wasp.json"
                ]
            ]
        )
    ]
  where
    -- NOTE: We manually create a fake `ExtImport` object
    -- This is so that `tsc` does not resolve any external imports
    -- besides the `@wasp.sh/spec` package.
    mainWaspTsWithError :: T.Text
    mainWaspTsWithError =
      [trimming|
        import { app, route } from "@wasp.sh/spec";

        export default app({
          name: "enti",
          title: "enti",
          wasp: { version: "$textWaspVersion" },
          head: ["<link rel='icon' href='/favicon.ico' />"],
          auth: {
            userEntity: "$invalidEntityName",
            methods: {
              usernameAndPassword: {},
            },
            onAuthFailedRedirectTo: "/",
          },
          parts: [
            route("Route", "/", {
              kind: "page",
              component: { from: "@src/somewhere", import: "something" },
            }),
          ],
        });
      |]

    prismaUserModel :: T.Text
    prismaUserModel =
      [trimming|
        model $validEntityName {
          id       String @id @default(uuid())
          username String
        }
      |]

    validEntityName :: T.Text
    validEntityName = "User"

    invalidEntityName :: T.Text
    invalidEntityName = "InvalidUser"

    textWaspVersion :: T.Text
    textWaspVersion = T.pack . show $ waspVersion
