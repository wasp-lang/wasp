module Wasp.Util.Terminal
  ( Style (..),
    applyStyles,
  )
where

data Style
  = Black
  | Red
  | Green
  | Yellow
  | Blue
  | Magenta
  | Cyan
  | White
  | Bold
  | Underline

-- | Given a string, returns decorated string that when printed in terminal
-- will have same content as original string but will also exibit specified styles.
applyStyles :: [Style] -> String -> String
applyStyles [] str = str
applyStyles _ "" = ""
applyStyles styles str = foldl applyStyle str styles ++ escapeCode ++ resetCode
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
styleCode Bold = "[1m"
styleCode Underline = "[4m"

escapeCode :: String
escapeCode = "\ESC"

resetCode :: String
resetCode = "[0m"
