{-# LANGUAGE QuasiQuotes #-}

module Tests.WaspModuleTest (waspModuleTest) where

import Control.Monad.Reader (ask)
import qualified Data.Text as T
import NeatInterpolation (trimming)
import ShellCommands
  ( ShellCommand,
    ShellCommandBuilder,
    TestContext (testCaseDir, waspProjectContext),
    WaspProjectContext (waspProjectDir),
    assertFileExists,
    buildShellCommand,
    createTestWaspProject,
    inTestWaspProjectDir,
    replaceMainWaspTsFile,
    setWaspDbToPSQL,
    shellSingleQuote,
    waspCliBuild,
    waspCliInstall,
    waspCliModuleBuild,
    waspCliModuleInstall,
    waspCliModuleNew,
    (~&&),
  )
import StrongPath (fromAbsDir, reldir, (</>))
import Test (Test (..), TestCase (..))
import Wasp.Cli.Command.CreateNewProject.AvailableTemplates (minimalStarterTemplate)
import Wasp.Version (waspVersion)

waspModuleTest :: Test
waspModuleTest =
  Test
    "wasp-module"
    [ TestCase
        "build-and-consume"
        ( sequence
            [ useIsolatedNpmCache,
              createTestWaspProject minimalStarterTemplate,
              createModule,
              inTestModuleDir
                [ waspCliModuleInstall,
                  waspCliModuleBuild,
                  return $ assertFileExists "dist/spec.js",
                  return $ assertFileExists "dist/spec.d.ts",
                  return $ assertFileExists "dist/MainPage.js",
                  return $ assertFileExists "dist/queries.js",
                  packModuleIntoHost,
                  assertPackedModuleExists
                ],
              inTestWaspProjectDir
                [ addModuleDependency,
                  waspCliInstall,
                  replaceMainWaspTsFile moduleHostSpec,
                  setWaspDbToPSQL,
                  waspCliBuild,
                  return $ assertFileExists "node_modules/@test/module/dist/spec.js"
                ]
            ]
        )
    ]

useIsolatedNpmCache :: ShellCommandBuilder TestContext ShellCommand
useIsolatedNpmCache = do
  context <- ask
  let npmCacheDir = context.testCaseDir </> [reldir|npm-cache|]
  return $ "export npm_config_cache=" ++ shellSingleQuote (fromAbsDir npmCacheDir)

createModule :: ShellCommandBuilder TestContext ShellCommand
createModule = waspCliModuleNew modulePackageName

inTestModuleDir :: [ShellCommandBuilder TestContext ShellCommand] -> ShellCommandBuilder TestContext ShellCommand
inTestModuleDir moduleCommandBuilders = do
  context <- ask
  let moduleDir = context.testCaseDir </> [reldir|test-module|]
  return $
    ("cd " ++ shellSingleQuote (fromAbsDir moduleDir))
      ~&& foldr1 (~&&) (buildShellCommand context $ sequence moduleCommandBuilders)
      ~&& ("cd " ++ shellSingleQuote (fromAbsDir context.testCaseDir))

packModuleIntoHost :: ShellCommandBuilder TestContext ShellCommand
packModuleIntoHost = do
  context <- ask
  let hostModulesDir = context.waspProjectContext.waspProjectDir </> [reldir|src/modules|]
  return $
    ("mkdir -p " ++ shellSingleQuote (fromAbsDir hostModulesDir))
      ~&& ("npm pack --pack-destination " ++ shellSingleQuote (fromAbsDir hostModulesDir))

assertPackedModuleExists :: ShellCommandBuilder TestContext ShellCommand
assertPackedModuleExists = do
  context <- ask
  let hostModulesDir = context.waspProjectContext.waspProjectDir </> [reldir|src/modules|]
  return $ assertFileExists $ fromAbsDir hostModulesDir ++ moduleTarballName

addModuleDependency :: ShellCommandBuilder WaspProjectContext ShellCommand
addModuleDependency =
  return $
    "npm pkg set "
      ++ show ("dependencies." ++ modulePackageName ++ "=file:src/modules/" ++ moduleTarballName)

moduleHostSpec :: T.Text
moduleHostSpec =
  [trimming|
    import { app } from "@wasp.sh/spec";
    import getModuleSpec from "@test/module/spec";

    export default app({
      name: "moduleHost",
      wasp: { version: "$textWaspVersion" },
      title: "Module host",
      spec: [getModuleSpec({ prefix: "/module" })],
    });
  |]

modulePackageName :: String
modulePackageName = "@test/module"

moduleTarballName :: String
moduleTarballName = "test-module-0.0.1.tgz"

textWaspVersion :: T.Text
textWaspVersion = T.pack . show $ waspVersion
