module Wasp.Util.Terminal
  ( Style (..),
    styleCode,
    applyStyles,
    getAnsiCodeFor,
    ansiEscapeCode,
    ansiResetCode,
  )
where

import Data.List (foldl')

-- | Applies the Wasp CLI standardized code styling to a string.
styleCode :: String -> String
styleCode = applyStyles [Bold]

data Style
  = Black
  | Red
  | Green
  | Yellow
  | Blue
  | Magenta
  | Cyan
  | White
  | BlackBg
  | RedBg
  | GreenBg
  | YellowBg
  | BlueBg
  | MagentaBg
  | CyanBg
  | WhiteBg
  | Bold
  | Underline
  | Blink
  deriving (Show, Eq)

-- | Given a string, returns decorated string that when printed in terminal
-- will have same content as original string but will also exibit specified styles.
applyStyles :: [Style] -> String -> String
applyStyles [] str = str
applyStyles _ "" = ""
applyStyles styles str = foldl' applyStyle str styles ++ ansiEscapeCode ++ ansiResetCode
  where
    applyStyle s style = ansiEscapeCode ++ getAnsiCodeFor style ++ s

getAnsiCodeFor :: Style -> String
getAnsiCodeFor Black = "[30m"
getAnsiCodeFor Red = "[31m"
getAnsiCodeFor Green = "[32m"
getAnsiCodeFor Yellow = "[33m"
getAnsiCodeFor Blue = "[34m"
getAnsiCodeFor Magenta = "[35m"
getAnsiCodeFor Cyan = "[36m"
getAnsiCodeFor White = "[37m"
getAnsiCodeFor BlackBg = "[40m"
getAnsiCodeFor RedBg = "[41m"
getAnsiCodeFor GreenBg = "[42m"
getAnsiCodeFor YellowBg = "[43m"
getAnsiCodeFor BlueBg = "[44m"
getAnsiCodeFor MagentaBg = "[45m"
getAnsiCodeFor CyanBg = "[46m"
getAnsiCodeFor WhiteBg = "[47m"
getAnsiCodeFor Bold = "[1m"
getAnsiCodeFor Underline = "[4m"
getAnsiCodeFor Blink = "[5m" -- Blink does not work in all terminal emulators (e.g. on mac in iTerm2).

ansiEscapeCode :: String
ansiEscapeCode = "\ESC"

ansiResetCode :: String
ansiResetCode = "[0m"
