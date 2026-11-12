module Wasp.Message (Message (..), Details (..), ToolOutput (..), SendMessage, textDetails, toolOutputDetails) where

-- This module defines a simple protocol for sending progress messages to a
-- UI (like the CLI).
-- The idea is that we want to report on phases that start, end either in
-- success or failure, and can send warnings along the way.
-- We use this protocol to give the generator (and in particular the setup phase)
-- the capability to report on progress messages.
-- This protocol is for sending messages for display in a UI.
-- If you need success or failure another purpose use return values.

data Message = Info String | Start String | Success String | Failure String Details | Warning String Details

-- | What a message says under its title: some text, and the output of a tool
-- Wasp ran, which the UI sets apart from the text.
data Details = Details
  { text :: String,
    toolOutput :: Maybe ToolOutput
  }

data ToolOutput = ToolOutput
  { command :: String,
    output :: String
  }

textDetails :: String -> Details
textDetails text' = Details text' Nothing

toolOutputDetails :: String -> String -> Details
toolOutputDetails command' output' = Details "" (Just $ ToolOutput command' output')

type SendMessage = Message -> IO ()
