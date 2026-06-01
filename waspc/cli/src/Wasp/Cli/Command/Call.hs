module Wasp.Cli.Command.Call where

-- | Coarse classification of which CLI command the user invoked.
-- Only the distinctions that telemetry cares about are encoded here.
data Call
  = Build
  | Deploy [String]
  | Other
