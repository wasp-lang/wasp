module Tests.ViteConfigTest (test_viteConfig) where

import E2E
import NeatInterpolation (trimming)
import Wasp.Cli.Command.CreateNewProject.AvailableTemplates (minimalStarterTemplate)

test_viteConfig :: TestTree
test_viteConfig =
  testGroup
    "vite-config-validation"
    [ test "fail-on-missing-vite-config" $
        withNewWaspProject minimalStarterTemplate $ do
          deleteFile viteConfigFile
          waspFails ["compile"] >>= assertOutputContains "Couldn't find `vite.config.ts`",
      test "fail-on-missing-wasp-plugin-import" $
        withNewWaspProject minimalStarterTemplate $ do
          writeToFile
            viteConfigFile
            [trimming|
              import { defineConfig } from "vite";

              export default defineConfig({});
            |]
          waspFails ["compile"] >>= assertOutputContains "doesn't seem to import the Wasp Vite plugin",
      test "fail-on-overriding-forced-options" $
        withNewWaspProject minimalStarterTemplate $ do
          writeToFile
            viteConfigFile
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
          wasp ["compile"]
          runExpectingFailure (withApiUrl $ viteBuildCommand [])
            >>= assertOutputContains "To serve your app from a subdirectory, set `client.baseDir` in your Wasp config."
    ]
