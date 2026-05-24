module Tests.WaspSpecEntityTypesTest (waspSpecEntityTypesTest) where

import qualified Data.Text as T
import NeatInterpolation (trimming)
import ShellCommands
  ( ShellCommand,
    ShellCommandBuilder,
    WaspProjectContext,
    appendToPrismaFile,
    assertCommandOutputContains,
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
        "typescript-errors-when-an-invalid-entity-name-is-provided"
        ( sequence
            [ createTestWaspProject minimalStarterTemplate,
              inTestWaspProjectDir
                [ appendToPrismaFile prismaUserModel,
                  replaceMainWaspTsFile validMainWaspTs,
                  waspCliCompile, -- Necessary to generate the spec's Entity types.
                  replaceMainWaspTsFile invalidMainWaspTs,
                  assertCommandOutputContains
                    (("! " ++) <$> typeCheckMainTsFiles)
                    ("Type '\"" ++ T.unpack invalidEntityName ++ "\"' is not assignable to type '\"" ++ T.unpack validEntityName ++ "\"'.")
                ]
            ]
        )
    ]
  where
    typeCheckMainTsFiles :: ShellCommandBuilder WaspProjectContext ShellCommand
    typeCheckMainTsFiles = return "npx tsc --noEmit -p tsconfig.wasp.json"

    prismaUserModel :: T.Text
    prismaUserModel =
      [trimming|
        model $validEntityName {
          id       String @id @default(uuid())
          username String
        }
      |]

    -- NOTE: We manually create a fake `ExtImport` object.
    -- This is so that `tsc` does not have to resolve any
    -- external imports besides the `@wasp.sh/spec` package.
    validMainWaspTs :: T.Text
    validMainWaspTs =
      [trimming|
        import { app, route } from "@wasp.sh/spec";

        export default app({
          name: "enti",
          title: "enti",
          wasp: { version: "$textWaspVersion" },
          head: ["<link rel='icon' href='/favicon.ico' />"],
          auth: {
            userEntity: "$validEntityName",
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

    validEntityName :: T.Text
    validEntityName = "User"

    -- NOTE: We manually create a fake `ExtImport` object.
    -- This is so that `tsc` does not have to resolve any
    -- external imports besides the `@wasp.sh/spec` package.
    invalidMainWaspTs :: T.Text
    invalidMainWaspTs =
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

    invalidEntityName :: T.Text
    invalidEntityName = "InvalidUser"

    textWaspVersion :: T.Text
    textWaspVersion = T.pack . show $ waspVersion
