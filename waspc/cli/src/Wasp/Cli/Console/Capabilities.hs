module Wasp.Cli.Console.Capabilities
  ( Capabilities (..),
    Surroundings (..),
    decideCapabilities,
  )
where

-- | What the console can do, decided once at startup.
data Capabilities = Capabilities
  { -- | Styling means colours (and anything else that only a terminal shows
    -- properly).
    canStyleOutput :: Bool,
    canPrompt :: Bool
  }
  deriving (Show, Eq)

-- | What Wasp runs in. Everything the capabilities depend on.
data Surroundings = Surroundings
  { isInputTerminal :: Bool,
    isOutputTerminal :: Bool,
    isOnCi :: Bool,
    -- | The value of the @TERM@ environment variable.
    terminalType :: Maybe String,
    -- | The value of the @NO_COLOR@ environment variable.
    noColor :: Maybe String
  }

decideCapabilities :: Surroundings -> Capabilities
decideCapabilities surroundings =
  Capabilities
    { canStyleOutput = isUserWatchingOutput && not isTerminalDumb && not isColorTurnedOff,
      -- A question written into a file or a CI log is a question nobody sees.
      canPrompt = surroundings.isInputTerminal && isUserWatchingOutput
    }
  where
    isUserWatchingOutput = surroundings.isOutputTerminal && not surroundings.isOnCi
    isTerminalDumb = surroundings.terminalType == Just "dumb"
    -- https://no-color.org/: turned off when present and not an empty string.
    isColorTurnedOff = maybe False (not . null) surroundings.noColor
