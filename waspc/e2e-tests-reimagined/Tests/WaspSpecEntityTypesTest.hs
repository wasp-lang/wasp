module Tests.WaspSpecEntityTypesTest (waspSpecEntityTypesTests) where

import qualified Data.Text as T
import E2e
import NeatInterpolation (trimming)
import Wasp.Cli.Command.CreateNewProject.AvailableTemplates (minimalStarterTemplate)

waspSpecEntityTypesTests :: [E2eTest]
waspSpecEntityTypesTests =
  testGroup
    "wasp-spec-entity-types"
    [ test "accepts-valid-entity-before-and-after-compile" $
        withNewWaspProject minimalStarterTemplate $ do
          appendToPrismaSchema userPrismaModel
          writeMainWaspTs $ mainWaspTs validEntityName
          run typeCheckMainWaspTs
          wasp ["compile"] -- Generates the spec's Entity types.
          run typeCheckMainWaspTs,
      -- NOTE: Rejecting an invalid entity before compile is currently impossible.
      -- See: https://github.com/wasp-lang/wasp/issues/4186
      test "rejects-invalid-entity-after-compile" $
        withNewWaspProject minimalStarterTemplate $ do
          appendToPrismaSchema userPrismaModel
          writeMainWaspTs $ mainWaspTs validEntityName
          wasp ["compile"] -- Generates the spec's Entity types.
          writeMainWaspTs $ mainWaspTs invalidEntityName
          runExpectingFailure typeCheckMainWaspTs
            >>= assertOutputContains ("Type '\"" <> invalidEntityName <> "\"' is not assignable to type '\"" <> validEntityName <> "\"'.")
    ]
  where
    typeCheckMainWaspTs = command "npx" ["tsc", "--noEmit", "-p", "tsconfig.wasp.json"]

    userPrismaModel :: T.Text
    userPrismaModel =
      [trimming|
        model $validEntityName {
          id       String @id @default(uuid())
          username String
        }
      |]

    -- NOTE: We use `ref(...)` so that `tsc` doesn't have to resolve any
    -- imports besides the `@wasp.sh/spec` package.
    mainWaspTs :: T.Text -> T.Text
    mainWaspTs entityName =
      [trimming|
        import { app, ref, route } from "@wasp.sh/spec";

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
