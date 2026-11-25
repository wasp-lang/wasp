module Wasp.Message (Message (..)) where

-- This module defines a simple protocol for sending progress messages to a
-- UI (like the CLI).
-- This protocol is for sending messages for display in a UI.
-- If you need success or failure another purpose use return values.

data Message
  = Info String
  | Success String
  | -- | What went wrong, and then the details: more about it, or how to fix it.
    -- The details can be empty.
    Failure String String
  | -- | Has the same parts as 'Failure'.
    Warning String String
