module Tests.WaspSpecEntityTypesTest (waspSpecEntityTypesTest) where

import Command (Command, cmd)
import qualified Data.Text as T
import NeatInterpolation (trimming)
import Steps
  ( appendToPrismaFile,
    assertCommandFailsWithOutputContaining,
    createTestWaspProject,
    inTestWaspProjectDir,
    replaceMainWaspTsFile,
    runCommand,
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
        [ createTestWaspProject minimalStarterTemplate,
          inTestWaspProjectDir
            [ appendToPrismaFile prismaUserModel,
              replaceMainWaspTsFile $ mainWaspTs validEntityName,
              runCommand compileWaspMainTsFiles
            ]
        ],
      TestCase
        "typescript-compiles-when-a-valid-entity-name-is-provided-after-compile"
        [ createTestWaspProject minimalStarterTemplate,
          inTestWaspProjectDir
            [ appendToPrismaFile prismaUserModel,
              replaceMainWaspTsFile $ mainWaspTs validEntityName,
              runCommand waspCliCompile, -- Necessary to generate the spec's Entity types.
              runCommand compileWaspMainTsFiles
            ]
        ],
      -- NOTE: Currently impossible. See: https://github.com/wasp-lang/wasp/issues/4186
      -- TestCase
      --   "typescript-errors-when-an-invalid-entity-name-is-provided-before-compile"
      --   [ createTestWaspProject minimalStarterTemplate,
      --     inTestWaspProjectDir
      --       [ appendToPrismaFile prismaUserModel,
      --         replaceMainWaspTsFile $ mainWaspTs invalidEntityName,
      --         assertCommandFailsWithOutputContaining
      --           compileWaspMainTsFiles
      --           ("Type '\"" ++ T.unpack invalidEntityName ++ "\"' is not assignable to type '\"" ++ T.unpack validEntityName ++ "\"'.")
      --       ]
      --   ],
      TestCase
        "typescript-errors-when-an-invalid-entity-name-is-provided-after-compile"
        [ createTestWaspProject minimalStarterTemplate,
          inTestWaspProjectDir
            [ appendToPrismaFile prismaUserModel,
              replaceMainWaspTsFile $ mainWaspTs validEntityName,
              runCommand waspCliCompile, -- Necessary to generate the spec's Entity types.
              replaceMainWaspTsFile $ mainWaspTs invalidEntityName,
              assertCommandFailsWithOutputContaining
                compileWaspMainTsFiles
                ("Type '\"" ++ T.unpack invalidEntityName ++ "\"' is not assignable to type '\"" ++ T.unpack validEntityName ++ "\"'.")
            ]
        ]
    ]
  where
    compileWaspMainTsFiles :: Command
    compileWaspMainTsFiles = cmd "npx" ["tsc", "--noEmit", "-p", "tsconfig.wasp.json"]

    prismaUserModel :: T.Text
    prismaUserModel =
      [trimming|
        model $validEntityName {
          id       String @id @default(uuid())
          username String
        }
      |]

    -- NOTE: We use `ref(...)` so that `tsc` does not have to resolve any
    -- external imports besides the `@wasp.sh/spec` package.
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

    textWaspVersion :: T.Text
    textWaspVersion = T.pack . show $ waspVersion
