module Wasp.Message (Message (..)) where

import Wasp.Problem (ToolOutput)

-- This module defines a simple protocol for sending progress messages to a
-- UI (like the CLI).
-- This protocol is for sending messages for display in a UI.
-- If you need success or failure another purpose use return values.

data Message
  = Info String
  | Success String
  | -- | What went wrong, then the details (more about it, or how to fix it),
    -- and then what the tool that ran into it printed, if it was a tool. The
    -- details can be empty.
    Failure String String (Maybe ToolOutput)
  | -- | Has the same parts as 'Failure'.
    Warning String String (Maybe ToolOutput)
