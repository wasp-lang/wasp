module Tests.WaspCompileTest (waspCompile) where

import GoldenTest (GoldenTest, makeGoldenTest)
import ShellCommands
  ( OutputDir (DevOutputDir),
    cdIntoCurrentProject,
    reformatPackageJson,
    waspCliCompile,
    waspCliNew,
  )

waspCompile :: GoldenTest
waspCompile =
  makeGoldenTest "waspCompile" $
    sequence
      [ waspCliNew,
        cdIntoCurrentProject,
        waspCliCompile,
        reformatPackageJson DevOutputDir
      ]
