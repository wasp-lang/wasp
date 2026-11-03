module Tests.WaspSpecEntityTypesTest (waspSpecEntityTypesTest) where

import qualified Data.Text as T
import E2E
import NeatInterpolation (trimming)
import Wasp.Cli.Command.CreateNewProject.AvailableTemplates (minimalStarterTemplate)

waspSpecEntityTypesTest :: E2ETest
waspSpecEntityTypesTest =
  testGroup
    "wasp-spec-entity-types"
    [ test "accepts-valid-entity-before-and-after-compile" $
        withNewWaspProject minimalStarterTemplate $ do
          appendToFile "schema.prisma" prismaUserModel
          writeToFile "main.wasp.ts" $ mainWaspTs validEntityName
          run compileWaspMainTsFiles
          wasp ["compile"] -- Necessary to generate the spec's Entity types.
          run compileWaspMainTsFiles,
      -- NOTE: Currently impossible to check that an invalid entity name is
      -- rejected before compiling. See: https://github.com/wasp-lang/wasp/issues/4186
      test "rejects-invalid-entity-after-compile" $
        withNewWaspProject minimalStarterTemplate $ do
          appendToFile "schema.prisma" prismaUserModel
          writeToFile "main.wasp.ts" $ mainWaspTs validEntityName
          wasp ["compile"] -- Necessary to generate the spec's Entity types.
          writeToFile "main.wasp.ts" $ mainWaspTs invalidEntityName
          runExpectingFailure compileWaspMainTsFiles
            >>= assertOutputContains ("Type '\"" <> invalidEntityName <> "\"' is not assignable to type '\"" <> validEntityName <> "\"'.")
    ]
  where
    compileWaspMainTsFiles :: Command
    compileWaspMainTsFiles = nodeModulesBin "tsc" ["--noEmit", "-p", "tsconfig.wasp.json"]

    prismaUserModel :: T.Text
    prismaUserModel =
      [trimming|
        model $validEntityName {
          id       String @id @default(uuid())
          username String
        }
      |]

    -- NOTE: We use `ref(...)` so that `tsc` does not have to resolve any
    -- external imports besides the `@wasp/spec` package.
    mainWaspTs :: T.Text -> T.Text
    mainWaspTs entityName =
      [trimming|
        import { app, ref, route } from "@wasp/spec";

        export default app({
          name: "enti",
          title: "enti",
          wasp: { version: "$textWaspVersion" },
          head: ["<link rel='icon' href='/favicon.ico' />"],
          auth: {
            userEntity: "$entityName",
            methods: {
              usernameAndPassword: {},
            },
            onAuthFailedRedirectTo: "/",
          },
          spec: [
            route("Route", "/", {
              kind: "page",
              component: ref({ from: "./src/somewhere", import: "something" }),
            }),
          ],
        });
      |]

    validEntityName, invalidEntityName :: T.Text
    validEntityName = "User"
    invalidEntityName = "InvalidUser"
