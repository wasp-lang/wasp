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
        "typescript-compiles-when-a-valid-entity-name-is-provided-before-compile"
        ( sequence
            [ createTestWaspProject minimalStarterTemplate,
              inTestWaspProjectDir
                [ appendToPrismaFile prismaUserModel,
                  replaceMainWaspTsFile $ mainWaspTs validEntityName,
                  compileWaspMainTsFiles
                ]
            ]
        ),
      TestCase
        "typescript-compiles-when-a-valid-entity-name-is-provided-after-compile"
        ( sequence
            [ createTestWaspProject minimalStarterTemplate,
              inTestWaspProjectDir
                [ appendToPrismaFile prismaUserModel,
                  replaceMainWaspTsFile $ mainWaspTs validEntityName,
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
      --             assertCommandOutputContains
      --               (("! " ++) <$> compileWaspMainTsFiles)
      --               ("Type '\"" ++ T.unpack invalidEntityName ++ "\"' is not assignable to type '\"" ++ T.unpack validEntityName ++ "\"'.")
      --           ]
      --       ]
      --   ),
      TestCase
        "typescript-errors-when-an-invalid-entity-name-is-provided-after-compile"
        ( sequence
            [ createTestWaspProject minimalStarterTemplate,
              inTestWaspProjectDir
                [ appendToPrismaFile prismaUserModel,
                  replaceMainWaspTsFile $ mainWaspTs validEntityName,
                  waspCliCompile, -- Necessary to generate the spec's Entity types.
                  replaceMainWaspTsFile $ mainWaspTs invalidEntityName,
                  assertCommandOutputContains
                    (("! " ++) <$> compileWaspMainTsFiles)
                    ("Type '\"" ++ T.unpack invalidEntityName ++ "\"' is not assignable to type '\"" ++ T.unpack validEntityName ++ "\"'.")
                ]
            ]
        )
    ]
  where
    compileWaspMainTsFiles :: ShellCommandBuilder WaspProjectContext ShellCommand
    compileWaspMainTsFiles = return "npx tsc --noEmit -p tsconfig.wasp.json"

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
    mainWaspTs :: T.Text -> T.Text
    mainWaspTs entityName =
      [trimming|
        import { app, route } from "@wasp.sh/spec";

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
          parts: [
            route("Route", "/", {
              kind: "page",
              component: { from: "@src/somewhere", import: "something" },
            }),
          ],
        });
      |]

    validEntityName, invalidEntityName :: T.Text
    validEntityName = "User"
    invalidEntityName = "InvalidUser"

    textWaspVersion :: T.Text
    textWaspVersion = T.pack . show $ waspVersion
