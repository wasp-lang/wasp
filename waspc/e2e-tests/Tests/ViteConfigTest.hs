{-# LANGUAGE OverloadedRecordDot #-}

module Tests.ViteConfigTest (viteConfigTest) where

import Control.Monad.Reader (ask)
import qualified Data.Text as T
import ShellCommands
  ( ShellCommand,
    ShellCommandBuilder,
    WaspNewTemplate (..),
    WaspProjectContext (..),
    createFile,
    createTestWaspProject,
    inTestWaspProjectDir,
    setWaspDbToPSQL,
    waspCliCompile,
  )
import StrongPath (relfile, (</>))
import Test (Test (..), TestCase (..))

viteConfigTest :: Test
viteConfigTest =
  Test
    "vite-config-validation"
    [ TestCase
        "fail-on-missing-vite-config"
        ( sequence
            [ createTestWaspProject Minimal,
              inTestWaspProjectDir
                [ setWaspDbToPSQL,
                  deleteViteConfig,
                  expectCommandFailure <$> waspCliCompile
                ]
            ]
        ),
      TestCase
        "fail-on-missing-wasp-plugin-import"
        ( sequence
            [ createTestWaspProject Minimal,
              inTestWaspProjectDir
                [ setWaspDbToPSQL,
                  writeViteConfigWithoutPlugin,
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
  createFile
    (context.waspProjectDir </> [relfile|vite.config.ts|])
    ( T.pack $
        unlines
          [ "import { defineConfig } from \"vite\";",
            "",
            "export default defineConfig({});"
          ]
    )

expectCommandFailure :: ShellCommand -> ShellCommand
expectCommandFailure command = "! " ++ command
