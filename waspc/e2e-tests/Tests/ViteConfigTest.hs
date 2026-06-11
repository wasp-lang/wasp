module Tests.ViteConfigTest (viteConfigTest) where

import Context (WaspProjectContext (..))
import NeatInterpolation (trimming)
import Step (Step, askStepContext)
import Steps
  ( createTestWaspProject,
    deleteFile,
    inTestWaspProjectDir,
    runCommandExpectingFailure,
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
    [ TestCase "fail-on-missing-vite-config" $ do
        createTestWaspProject minimalStarterTemplate
        inTestWaspProjectDir $ do
          deleteFile "vite.config.ts"
          runCommandExpectingFailure waspCliCompile,
      TestCase "fail-on-missing-wasp-plugin-import" $ do
        createTestWaspProject minimalStarterTemplate
        inTestWaspProjectDir $ do
          writeViteConfigWithoutPlugin
          runCommandExpectingFailure waspCliCompile
    ]

writeViteConfigWithoutPlugin :: Step WaspProjectContext ()
writeViteConfigWithoutPlugin = do
  context <- askStepContext
  writeToFile
    (context.waspProjectDir </> [relfile|vite.config.ts|])
    [trimming|
      import { defineConfig } from "vite";

      export default defineConfig({});
    |]
