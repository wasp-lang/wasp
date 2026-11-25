module Wasp.Problem
  ( Problem (..),
    ToolOutput (..),
    problem,
    problemWithToolOutput,
  )
where

-- | An error or a warning for the user.
data Problem = Problem
  { description :: String,
    -- | What the tool that ran into the problem printed, if it was a tool
    -- that ran into it.
    toolOutput :: Maybe ToolOutput
  }
  deriving (Eq, Show)

-- | What a tool that Wasp ran printed before it failed.
data ToolOutput = ToolOutput
  { -- | The tool as the user knows it (e.g. "npm install"), not the exact
    -- command line.
    command :: String,
    exitCode :: Int,
    output :: String
  }
  deriving (Eq, Show)

problem :: String -> Problem
problem problemDescription = Problem {description = problemDescription, toolOutput = Nothing}

problemWithToolOutput :: String -> ToolOutput -> Problem
problemWithToolOutput problemDescription failedToolOutput =
  Problem {description = problemDescription, toolOutput = Just failedToolOutput}
