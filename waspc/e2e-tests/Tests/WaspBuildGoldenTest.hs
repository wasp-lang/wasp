module Tests.WaspBuildGoldenTest (waspBuildGoldenTest) where

import GoldenTest.Runner (GoldenTest, makeGoldenTest)
import GoldenTest.ShellCommands
  ( createGoldenTestWaspApp,
    withInGoldenTestWaspAppDir,
  )
import WaspApp.ShellCommands
  ( buildWaspDockerImage,
    setWaspDbToPSQL,
    waspCliBuild,
  )

waspBuildGoldenTest :: GoldenTest
waspBuildGoldenTest =
  makeGoldenTest
    "wasp-build"
    [ createGoldenTestWaspApp,
      withInGoldenTestWaspAppDir
        [ setWaspDbToPSQL,
          waspCliBuild,
          buildWaspDockerImage
        ]
    ]
