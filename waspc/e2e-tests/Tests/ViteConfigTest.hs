module Tests.ViteConfigTest (viteConfigTest) where

import Control.Monad.Reader (ask)
import NeatInterpolation (trimming)
import ShellCommands
  ( ShellCommand,
    ShellCommandBuilder,
    WaspProjectContext (..),
    assertCommandOutputContains,
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
                  expectCommandFailure <$> waspCliCompile
                ]
            ]
        ),
      TestCase
        "fail-on-missing-wasp-plugin-import"
        ( sequence
            [ createTestWaspProject minimalStarterTemplate,
              inTestWaspProjectDir
                [ writeViteConfigWithoutPlugins,
                  expectCommandFailure <$> waspCliCompile
                ]
            ]
        ),
      TestCase
        "fail-on-missing-wasp-server-plugin-import"
        ( sequence
            [ createTestWaspProject minimalStarterTemplate,
              inTestWaspProjectDir
                [ writeViteConfigWithoutServerPlugin,
                  expectCommandFailure <$> waspCliCompile
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
                  assertCommandOutputContains
                    (expectCommandFailure <$> viteBuild)
                    "To serve your app from a subdirectory, set `client.baseDir` in your Wasp config."
                ]
            ]
        )
    ]

deleteViteConfig :: ShellCommandBuilder WaspProjectContext ShellCommand
deleteViteConfig = return "rm vite.config.ts"

writeViteConfigWithoutPlugins :: ShellCommandBuilder WaspProjectContext ShellCommand
writeViteConfigWithoutPlugins = do
  context <- ask
  writeToFile
    (context.waspProjectDir </> [relfile|vite.config.ts|])
    [trimming|
      import { defineConfig } from "vite";

      export default defineConfig({});
    |]

writeViteConfigWithoutServerPlugin :: ShellCommandBuilder WaspProjectContext ShellCommand
writeViteConfigWithoutServerPlugin = do
  context <- ask
  writeToFile
    (context.waspProjectDir </> [relfile|vite.config.ts|])
    [trimming|
      import { defineConfig } from "vite";
      import { wasp } from "wasp/client/vite";

      export default defineConfig({
        plugins: [wasp()],
      });
    |]

writeViteConfigOverridingForcedOptions :: ShellCommandBuilder WaspProjectContext ShellCommand
writeViteConfigOverridingForcedOptions = do
  context <- ask
  writeToFile
    (context.waspProjectDir </> [relfile|vite.config.ts|])
    [trimming|
      import { defineConfig } from "vite";
      import { wasp } from "wasp/client/vite";
      import { waspServer } from "wasp/server/vite";

      export default defineConfig({
        plugins: [wasp(), waspServer()],
        base: "/my-subdir/",
        envPrefix: "MY_APP_",
        build: {
          outDir: "dist",
        },
      });
    |]

viteBuild :: ShellCommandBuilder WaspProjectContext ShellCommand
viteBuild = return "npx vite build"

expectCommandFailure :: ShellCommand -> ShellCommand
expectCommandFailure command = "! " ++ command
