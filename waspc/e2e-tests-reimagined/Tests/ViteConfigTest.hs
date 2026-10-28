module Tests.ViteConfigTest (viteConfigTests) where

import E2e
import NeatInterpolation (trimming)
import Wasp.Cli.Command.CreateNewProject.AvailableTemplates (minimalStarterTemplate)

viteConfigTests :: [E2eTest]
viteConfigTests =
  testGroup
    "vite-config-validation"
    [ test "fail-on-missing-vite-config" $
        withNewWaspProject minimalStarterTemplate $ do
          deleteFile "vite.config.ts"
          waspFails ["compile"],
      test "fail-on-missing-wasp-plugin-import" $
        withNewWaspProject minimalStarterTemplate $ do
          writeToFile
            "vite.config.ts"
            [trimming|
              import { defineConfig } from "vite";

              export default defineConfig({});
            |]
          waspFails ["compile"],
      test "fail-on-overriding-forced-options" $
        withNewWaspProject minimalStarterTemplate $ do
          writeToFile
            "vite.config.ts"
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
          runExpectingFailure (command "npx" ["vite", "build"])
            >>= assertOutputContains "To serve your app from a subdirectory, set `client.baseDir` in your Wasp config."
    ]
