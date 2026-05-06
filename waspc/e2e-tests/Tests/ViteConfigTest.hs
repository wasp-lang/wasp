{-# LANGUAGE OverloadedRecordDot #-}

module Tests.ViteConfigTest (viteConfigTest) where

import Control.Monad.Reader (ask)
import NeatInterpolation (trimming)
import ShellCommands
  ( ShellCommand,
    ShellCommandBuilder,
    WaspProjectContext (..),
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
                [ writeViteConfigWithoutPlugin,
                  expectCommandFailure <$> waspCliCompile
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

expectCommandFailure :: ShellCommand -> ShellCommand
expectCommandFailure command = "! " ++ command
