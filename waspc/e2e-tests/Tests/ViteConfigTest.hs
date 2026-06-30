module Tests.ViteConfigTest (viteConfigTest) where

import Context (WaspProjectContext (..))
import NeatInterpolation (trimming)
import Step (Step, askStepContext)
import Steps
  ( createWaspProject,
    deleteFile,
    inWaspProjectDir,
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
        createWaspProject minimalStarterTemplate
        inWaspProjectDir $ do
          deleteFile "vite.config.ts"
          runCommandExpectingFailure waspCliCompile,
      TestCase "fail-on-missing-wasp-plugin-import" $ do
        createWaspProject minimalStarterTemplate
        inWaspProjectDir $ do
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
