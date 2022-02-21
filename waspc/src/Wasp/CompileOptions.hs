module Wasp.CompileOptions
  ( CompileOptions (..),
  )
where

import StrongPath (Abs, Dir, Path')
import Wasp.AppSpec.ExternalCode (SourceExternalCodeDir)
import Wasp.Message (SendMessage)

-- TODO(martin): Should these be merged with Wasp data? Is it really a separate thing or not?
--   It would be easier to pass around if it is part of Wasp data. But is it semantically correct?
--   Maybe it is, even more than this!
data CompileOptions = CompileOptions
  { externalCodeDirPath :: !(Path' Abs (Dir SourceExternalCodeDir)),
    isBuild :: !Bool,
    -- We give the compiler the ability to send messages. The code that
    -- invokes the compiler (such as the CLI) can then implement a way
    -- to display these messages.
    sendMessage :: SendMessage
  }
