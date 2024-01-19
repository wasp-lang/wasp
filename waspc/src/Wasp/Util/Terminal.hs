module Wasp.Util.Terminal
  ( Style (..),
    applyStyles,
    styleCode,
    escapeCode,
    resetCode,
  )
where

import Data.List (foldl')

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
applyStyles styles str = foldl' applyStyle str styles ++ escapeCode ++ resetCode
  where
    applyStyle s style = escapeCode ++ styleCode style ++ s

styleCode :: Style -> String
styleCode Black = "[30m"
styleCode Red = "[31m"
styleCode Green = "[32m"
styleCode Yellow = "[33m"
styleCode Blue = "[34m"
styleCode Magenta = "[35m"
styleCode Cyan = "[36m"
styleCode White = "[37m"
styleCode BlackBg = "[40m"
styleCode RedBg = "[41m"
styleCode GreenBg = "[42m"
styleCode YellowBg = "[43m"
styleCode BlueBg = "[44m"
styleCode MagentaBg = "[45m"
styleCode CyanBg = "[46m"
styleCode WhiteBg = "[47m"
styleCode Bold = "[1m"
styleCode Underline = "[4m"
styleCode Blink = "[5m" -- Blink does not work in all terminal emulators (e.g. on mac in iTerm2).

escapeCode :: String
escapeCode = "\ESC"

resetCode :: String
resetCode = "[0m"
