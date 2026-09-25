module Tests.SdkOperationTypesTest (sdkOperationTypesTest) where

import ShellCommands
  ( ShellCommand,
    ShellCommandBuilder,
    WaspProjectContext,
    assertCommandOutputContains,
    inTestWaspProjectDir,
    waspCliCompile,
  )
import Test (Test (..), TestCase (..))

sdkOperationTypesTest :: Test
sdkOperationTypesTest =
  Test
    "sdk-operation-types"
    [ TestCase
        "detects-and-recovers-operation-return-type-errors-incrementally"
        ( sequence
            [ return "cp -R ../../snapshots/kitchen-sink-current/wasp-app .",
              inTestWaspProjectDir
                [ return $ "cp " ++ queriesFile ++ " ../queries.ts",
                  waspCliCompile,
                  bundleServer,
                  return $ "sed 's/return tasks;/return [];/' ../queries.ts > " ++ queriesFile,
                  -- Editing the consumers makes TypeScript report the initial errors.
                  return "printf '\\n' >> src/features/operations/components/Todo.tsx",
                  return "printf '\\n' >> src/features/operations/components/Todo.test.tsx",
                  waspCliCompile,
                  assertCommandOutputContains
                    (("! " ++) <$> bundleServer)
                    "Property 'id' does not exist on type 'never'.",
                  return $ "cp ../queries.ts " ++ queriesFile,
                  waspCliCompile,
                  bundleServer
                ]
            ]
        )
    ]
  where
    queriesFile :: FilePath
    queriesFile = "src/features/operations/queries.ts"

    bundleServer :: ShellCommandBuilder WaspProjectContext ShellCommand
    bundleServer = return "npm --prefix .wasp/out/server run bundle"
