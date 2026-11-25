module Wasp.CompileOptions
  ( CompileOptions (..),
  )
where

import StrongPath (Abs, Dir, Path')
import Wasp.Generator.Setup (SetupGoal)
import qualified Wasp.Project.BuildType as BuildType
import Wasp.Project.Common (WaspProjectDir)
import Wasp.TaskRunner (TaskRunner)

-- TODO(martin): Should these be merged with Wasp data? Is it really a separate thing or not?
--   It would be easier to pass around if it is part of Wasp data. But is it semantically correct?
--   Maybe it is, even more than this!
data CompileOptions = CompileOptions
  { waspProjectDir :: !(Path' Abs (Dir WaspProjectDir)),
    buildType :: !BuildType.BuildType,
    -- We give the compiler the ability to report the progress of its steps.
    -- The code that invokes the compiler (such as the CLI) can then implement
    -- a way to display it.
    taskRunner :: TaskRunner,
    -- What the generated app needs to be set up for after the code is
    -- generated (Prisma CLI, SDK, ...).
    setupGoals :: [SetupGoal]
  }
