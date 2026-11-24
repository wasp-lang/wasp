module Wasp.Util.TextStyle
  ( Style (..),
    styleCode,
    applyStyles,
    getAnsiCodeFor,
    ansiEscapeCode,
    ansiResetAllCode,
    styleText,
    styleTitle,
    styleSuccess,
    styleError,
    styleWarning,
    styleNotice,
    styleInfo,
    styleHighlight,
    styleSubtle,
    styleMessage,
    styleAccent,
    styleCommand,
    styleDate,
    stripStyles,
  )
where

import Data.List (foldl', stripPrefix)

-- | Applies the Wasp CLI standardized code styling to a string.
styleCode :: String -> String
styleCode "" = ""
styleCode str = ansiEscapeCode ++ getAnsiCodeFor Bold ++ str ++ ansiEscapeCode ++ ansiResetBoldDimCode

-- | Applies the Wasp CLI standardized text styling to a string. Currently, this
-- is a no-op, but it exists for future-proofing. And to make consecutive calls
-- to styleCode and styleText look similar.
styleText :: String -> String
styleText = id

styleTitle :: String -> String
styleTitle = applyStyles [Bold]

styleSuccess :: String -> String
styleSuccess = applyStyles [Green]

styleError :: String -> String
styleError = applyStyles [Red]

styleWarning :: String -> String
styleWarning = applyStyles [Yellow]

-- | For something the user should notice, but that isn't a problem yet.
styleNotice :: String -> String
styleNotice = applyStyles [Magenta]

styleInfo :: String -> String
styleInfo = applyStyles [Blue]

styleHighlight :: String -> String
styleHighlight = applyStyles [Cyan]

-- | For text that matters less than the text around it.
styleSubtle :: String -> String
styleSubtle = applyStyles [Grey]

-- | For what Wasp itself says about its work.
styleMessage :: String -> String
styleMessage = applyStyles [Yellow]

-- | For a small part that should stand out from the plain text around it.
styleAccent :: String -> String
styleAccent = applyStyles [Yellow]

-- | For the name of a command in a list of commands.
styleCommand :: String -> String
styleCommand = applyStyles [Yellow, Bold]

styleDate :: String -> String
styleDate = applyStyles [Bold, Yellow]

-- | Removes everything that the functions in this module add, so that only
-- the text is left. For output that isn't shown in a terminal.
stripStyles :: String -> String
stripStyles "" = ""
stripStyles text@(character : afterCharacter) = case stripPrefix ansiControlSequenceIntroducer text of
  Just afterIntroducer -> stripStyles $ drop 1 $ dropWhile (not . isFinalCharacter) afterIntroducer
  Nothing -> character : stripStyles afterCharacter
  where
    ansiControlSequenceIntroducer = ansiEscapeCode ++ "["
    isFinalCharacter c = c >= '@' && c <= '~'

data Style
  = Black
  | Red
  | Green
  | Yellow
  | Blue
  | Magenta
  | Cyan
  | White
  | Grey
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
applyStyles styles str = foldl' applyStyle str styles ++ ansiEscapeCode ++ ansiResetAllCode
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
getAnsiCodeFor Grey = "[90m"
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

ansiResetAllCode :: String
ansiResetAllCode = "[0m"

ansiResetBoldDimCode :: String
ansiResetBoldDimCode = "[22m"
