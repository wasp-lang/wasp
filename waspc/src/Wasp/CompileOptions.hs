module Wasp.CompileOptions
  ( CompileOptions (..),
  )
where

import StrongPath (Abs, Dir, Path')
import Wasp.Generator.Setup (SetupGoal)
import Wasp.Message (SendMessage)
import qualified Wasp.Project.BuildType as BuildType
import Wasp.Project.Common (WaspProjectDir)
import Wasp.Util.IO.Process (WriteOutput)

-- TODO(martin): Should these be merged with Wasp data? Is it really a separate thing or not?
--   It would be easier to pass around if it is part of Wasp data. But is it semantically correct?
--   Maybe it is, even more than this!
data CompileOptions = CompileOptions
  { waspProjectDir :: !(Path' Abs (Dir WaspProjectDir)),
    buildType :: !BuildType.BuildType,
    -- We give the compiler the ability to send messages. The code that
    -- invokes the compiler (such as the CLI) can then implement a way
    -- to display these messages.
    sendMessage :: SendMessage,
    -- Where the jobs that the compiler runs (npm install, Prisma, the type
    -- check, ...) write their output.
    --
    -- FIXME: The compiler shouldn't need this. Its jobs print their output as
    -- they run, so the caller has to tell them where to. Once they capture
    -- the output and return it when they fail, remove this field and stop
    -- passing the function down to every job.
    writeJobOutput :: WriteOutput,
    -- What the generated app needs to be set up for after the code is
    -- generated (Prisma CLI, SDK, ...).
    setupGoals :: [SetupGoal]
  }
