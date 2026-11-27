module Tests.ViteConfigTest (viteConfigTest) where

import Control.Monad.Reader (ask)
import NeatInterpolation (trimming)
import ShellCommands
  ( ShellCommand,
    ShellCommandBuilder,
    WaspProjectContext (..),
    assertCommandFails,
    assertCommandFailsWith,
    createTestWaspProject,
    inTestWaspProjectDir,
    waspCliCompile,
    writeToFile,
  )
import StrongPath (relfile, (</>))
import Test (Test (..), TestCase (..))
import Wasp.Cli.Command.CreateNewProject.AvailableTemplates (minimalStarterTemplate)

viteConfigTest :: Test
viteConfigTest =
  Test
    "vite-config-validation"
    [ TestCase
        "fail-on-missing-vite-config"
        ( sequence
            [ createTestWaspProject minimalStarterTemplate,
              inTestWaspProjectDir
                [ deleteViteConfig,
                  assertCommandFails waspCliCompile
                ]
            ]
        ),
      TestCase
        "fail-on-missing-wasp-plugin-import"
        ( sequence
            [ createTestWaspProject minimalStarterTemplate,
              inTestWaspProjectDir
                [ writeViteConfigWithoutPlugin,
                  assertCommandFails waspCliCompile
                ]
            ]
        ),
      TestCase
        "fail-on-overriding-forced-options"
        ( sequence
            [ createTestWaspProject minimalStarterTemplate,
              inTestWaspProjectDir
                [ writeViteConfigOverridingForcedOptions,
                  waspCliCompile,
                  assertCommandFailsWith
                    "To serve your app from a subdirectory, set `client.baseDir` in your Wasp config."
                    viteBuild
                ]
            ]
        )
    ]

deleteViteConfig :: ShellCommandBuilder WaspProjectContext ShellCommand
deleteViteConfig = return "rm vite.config.ts"

writeViteConfigWithoutPlugin :: ShellCommandBuilder WaspProjectContext ShellCommand
writeViteConfigWithoutPlugin = do
  context <- ask
  writeToFile
    (context.waspProjectDir </> [relfile|vite.config.ts|])
    [trimming|
      import { defineConfig } from "vite";

      export default defineConfig({});
    |]

writeViteConfigOverridingForcedOptions :: ShellCommandBuilder WaspProjectContext ShellCommand
writeViteConfigOverridingForcedOptions = do
  context <- ask
  writeToFile
    (context.waspProjectDir </> [relfile|vite.config.ts|])
    [trimming|
      import { defineConfig } from "vite";
      import { wasp } from "wasp/client/vite";

      export default defineConfig({
        plugins: [wasp()],
        base: "/my-subdir/",
        envPrefix: "MY_APP_",
        build: {
          outDir: "dist",
        },
      });
    |]

viteBuild :: ShellCommandBuilder WaspProjectContext ShellCommand
viteBuild = return "npx vite build"
