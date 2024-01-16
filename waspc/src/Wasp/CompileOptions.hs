module Wasp.CompileOptions
  ( CompileOptions (..),
  )
where

import StrongPath (Abs, Dir, Path')
import Wasp.Generator.Monad (GeneratorWarning)
import Wasp.Message (SendMessage)
import Wasp.Project.Common (WaspProjectDir)

-- TODO(martin): Should these be merged with Wasp data? Is it really a separate thing or not?
--   It would be easier to pass around if it is part of Wasp data. But is it semantically correct?
--   Maybe it is, even more than this!
data CompileOptions = CompileOptions
  { waspProjectDirPath :: !(Path' Abs (Dir WaspProjectDir)),
    isBuild :: !Bool,
    -- We give the compiler the ability to send messages. The code that
    -- invokes the compiler (such as the CLI) can then implement a way
    -- to display these messages.
    sendMessage :: SendMessage,
    -- The generator returns a list of warnings and errors that happen during compilation.
    -- CLI commands will almost always compile before they execute to ensure the project is up to date.
    -- This filter function allows callers to ignore certain warnings where they do not make sense.
    -- For example, showing a compilation warning to run `db migrate-dev` when you are running that command.
    generatorWarningsFilter :: [GeneratorWarning] -> [GeneratorWarning]
  }
