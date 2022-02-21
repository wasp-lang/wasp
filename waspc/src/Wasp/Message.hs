module Wasp.Message (Message (..), SendMessage) where

-- This module defines a simple protocol for sending progress messages to a
-- UI (like the CLI).
-- The idea is that we want to report on phases that start, end either in
-- success or failure, and can send warnings along the way.
-- We use this protocol to give the generator (and in particular the setup phase)
-- the capability to report on progress messages.
-- This protocol is for sending messages for display in a UI.
-- If you need success or failure another purpose use return values.

data Message = Start String | Success String | Failure String | Warning String

type SendMessage = Message -> IO ()