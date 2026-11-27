module Tests.WaspSpecEntityTypesTest (waspSpecEntityTypesTest) where

import qualified Data.Text as T
import NeatInterpolation (trimming)
import ShellCommands
  ( ShellCommand,
    ShellCommandBuilder,
    WaspProjectContext,
    appendToPrismaFile,
    assertCommandFailsWith,
    createTestWaspProject,
    inTestWaspProjectDir,
    installOptionalFeaturePackagesCommand,
    replaceMainWaspTsFile,
    waspCliCompile,
  )
import Test (Test (..), TestCase (..))
import Wasp.Cli.Command.CreateNewProject.AvailableTemplates (minimalStarterTemplate)
import Wasp.Generator.SdkGenerator.Server.AuthG (authNpmDeps)

waspSpecEntityTypesTest :: Test
waspSpecEntityTypesTest =
  Test
    "wasp-spec-entity-types"
    [ TestCase
        "accepts-valid-entity-before-and-after-compile"
        ( sequence
            [ createTestWaspProject minimalStarterTemplate,
              inTestWaspProjectDir
                [ appendToPrismaFile prismaUserModel,
                  replaceMainWaspTsFile $ mainWaspTs validEntityName,
                  compileWaspMainTsFiles,
                  installAuthPackages,
                  waspCliCompile, -- Necessary to generate the spec's Entity types.
                  compileWaspMainTsFiles
                ]
            ]
        ),
      -- NOTE: Currently impossible. See: https://github.com/wasp-lang/wasp/issues/4186
      -- TestCase
      --   "typescript-errors-when-an-invalid-entity-name-is-provided-before-compile"
      --   ( sequence
      --       [ createTestWaspProject minimalStarterTemplate,
      --         inTestWaspProjectDir
      --           [ appendToPrismaFile prismaUserModel,
      --             replaceMainWaspTsFile $ mainWaspTs invalidEntityName,
      --             assertCommandFailsWith
      --               ("Type '\"" ++ T.unpack invalidEntityName ++ "\"' is not assignable to type '\"" ++ T.unpack validEntityName ++ "\"'.")
      --               compileWaspMainTsFiles
      --           ]
      --       ]
      --   ),
      TestCase
        "rejects-invalid-entity-after-compile"
        ( sequence
            [ createTestWaspProject minimalStarterTemplate,
              inTestWaspProjectDir
                [ appendToPrismaFile prismaUserModel,
                  replaceMainWaspTsFile $ mainWaspTs validEntityName,
                  installAuthPackages,
                  waspCliCompile, -- Necessary to generate the spec's Entity types.
                  replaceMainWaspTsFile $ mainWaspTs invalidEntityName,
                  assertCommandFailsWith
                    ("Type '\"" ++ T.unpack invalidEntityName ++ "\"' is not assignable to type '\"" ++ T.unpack validEntityName ++ "\"'.")
                    compileWaspMainTsFiles
                ]
            ]
        )
    ]
  where
    compileWaspMainTsFiles :: ShellCommandBuilder WaspProjectContext ShellCommand
    compileWaspMainTsFiles = return "npx tsc --noEmit -p tsconfig.wasp.json"

    installAuthPackages :: ShellCommandBuilder WaspProjectContext ShellCommand
    installAuthPackages = return $ installOptionalFeaturePackagesCommand [authNpmDeps]

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
