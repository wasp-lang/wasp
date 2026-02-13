{-# LANGUAGE OverloadedRecordDot #-}

module Tests.ViteOptionsValidationTest (viteOptionsValidationTest) where

import Control.Monad.Reader (ask)
import NeatInterpolation (trimming)
import ShellCommands
  ( ShellCommand,
    ShellCommandBuilder,
    WaspNewTemplate (..),
    WaspProjectContext (..),
    createFile,
    createTestWaspProject,
    inTestWaspProjectDir,
    waspCliStart,
  )
import StrongPath (relfile, (</>))
import Test (Test (..), TestCase (..))

viteOptionsValidationTest :: Test
viteOptionsValidationTest =
  Test
    "vite-options-validation"
    [ TestCase
        "fail-overriding-base-option"
        ( sequence
            [ createTestWaspProject Minimal,
              inTestWaspProjectDir
                [ writeViteConfigWithForcedOption,
                  expectCommandFailure <$> waspCliStart
                ]
            ]
        )
    ]
  where
    writeViteConfigWithForcedOption :: ShellCommandBuilder WaspProjectContext ShellCommand
    writeViteConfigWithForcedOption = do
      waspProjectContext <- ask
      createFile
        (waspProjectContext.waspProjectDir </> [relfile|vite.config.ts|])
        [trimming|
          import { defineConfig } from "vite";
          import { wasp } from "wasp/client/vite";

          export default defineConfig({
            plugins: [wasp()],
            base: "/other",
          });
        |]

    expectCommandFailure :: ShellCommand -> ShellCommand
    expectCommandFailure command = "! " ++ command
