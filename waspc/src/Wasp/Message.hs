module Wasp.Message (Message (..), Entry (..), Part (..), SendMessage) where

-- This module defines a simple protocol for sending progress messages to a
-- UI (like the CLI).
-- The idea is that we want to report on phases that start, end either in
-- success or failure, and can send warnings along the way.
-- We use this protocol to give the generator (and in particular the setup phase)
-- the capability to report on progress messages.
-- This protocol is for sending messages for display in a UI.
-- If you need success or failure another purpose use return values.

data Message = Info String | Start String | Success String | Failure [Entry] | Warning [Entry]

-- | One error or warning: what went wrong, then anything more there is to say about it.
data Entry = Entry String [Part]
  deriving (Show, Eq)

data Part
  = Text String
  | -- | The output of a command Wasp ran: the command, its exit code, then what it printed.
    ToolOutput String Int String
  deriving (Show, Eq)

type SendMessage = Message -> IO ()
