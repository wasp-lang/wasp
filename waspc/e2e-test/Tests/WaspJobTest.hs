module Tests.WaspJobTest (waspJob) where

import GoldenTest (GoldenTest, makeGoldenTest)
import ShellCommands
  ( appendToWaspFile,
    cdIntoCurrentProject,
    createFile,
    waspCliCompile,
    waspCliNew,
  )

waspJob :: GoldenTest
waspJob = do
  let entityDecl =
        " job MySpecialJob { \n\
        \   perform: import { foo } from \"@ext/jobs/bar.js\"  \n\
        \ } \n"

  let jobFile =
        " export const foo = async (args) => { \n\
        \   return 1 \n\
        \ } \n"

  makeGoldenTest "waspJob" $
    sequence
      [ waspCliNew,
        cdIntoCurrentProject,
        appendToWaspFile entityDecl,
        createFile jobFile "./ext/jobs" "bar.js",
        waspCliCompile
      ]
