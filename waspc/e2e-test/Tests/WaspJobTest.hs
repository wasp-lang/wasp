module Tests.WaspJobTest (waspJob) where

import GoldenTest (GoldenTest, makeGoldenTest)
import ShellCommands
  ( appendToWaspFile,
    cdIntoCurrentProject,
    createFile,
    setDbToPSQL,
    waspCliCompile,
    waspCliNew,
  )

waspJob :: GoldenTest
waspJob = do
  let jobDecl =
        " job MySpecialJob { \n\
        \   executor: PgBoss, \n\
        \   perform: { \n\
        \     fn: import { foo } from \"@server/jobs/bar.js\"  \n\
        \   } \n\
        \ } \n"

  let jobFile =
        " export const foo = async (args) => { \n\
        \   return 1 \n\
        \ } \n"

  makeGoldenTest "waspJob" $
    sequence
      [ waspCliNew,
        cdIntoCurrentProject,
        setDbToPSQL,
        appendToWaspFile jobDecl,
        createFile jobFile "./src/server/jobs" "bar.js",
        waspCliCompile
      ]
