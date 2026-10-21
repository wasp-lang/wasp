{-# LANGUAGE FlexibleInstances #-}

module Wasp.Cli.Interactive
  ( InteractiveTerminal,
    getInteractiveTerminal,
    isPromptInputEOF,
    askForInput,
    askToChoose,
    askForRequiredInput,
    tryGettingConfirmation,
    askForConfirmation,
    tryGettingConfirmationWithTimeout,
    IsOption (..),
    ConfirmationError (..),
    Option (..),
  )
where

import Control.Applicative ((<|>))
import Data.Char (toLower)
import Data.Foldable (find)
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.List (intercalate)
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.List.NonEmpty as NE
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import System.IO (Handle, hFlush, hGetLine, hIsTerminalDevice, hPutStr, hPutStrLn, stdin, stdout)
import System.IO.Error (ioeGetHandle, isEOFError)
import System.Timeout (timeout)
import Text.Read (readMaybe)
import qualified Wasp.Util.Terminal as Term

-- This is the only way to get an interactive terminal.
-- The caller can decide what to do if they can't get one: fail with a way to
-- pass the answers as arguments, skip the question, etc.
getInteractiveTerminal :: IO (Maybe InteractiveTerminal)
getInteractiveTerminal = do
  isTerminal <- hIsTerminalDevice stdioTerminal.inputHandle
  return $
    if isTerminal
      -- If you change the terminal here, change it in `isPromptInputEOF` too.
      then Just stdioTerminal
      else Nothing

-- | True for an error caused by the user ending an input (Ctrl+D) while a
-- prompt is waiting for an answer.
isPromptInputEOF :: IOError -> Bool
-- If you change the terminal here, change it in `getInteractiveTerminal` too.
isPromptInputEOF e = isEOFError e && ioeGetHandle e == Just stdioTerminal.inputHandle

{-
  Why are we doing this?
  Using a list of Strings for options results in the Strings being wrapped in quotes
  when printed.

  What we want to avoid:
    Choose an option:
    - "one"
    - "two"

  What we want:
    Choose an option:
    - one
    - two

  We want to avoid this so users can type the name of the option when answering
  without having to type the quotes as well.

  We introduced the IsOption class to get different "show" behavior for Strings and other
  types. If we are using something other then String, an instance of IsOption needs to be defined,
  but for Strings it just returns the String itself.
-}
class IsOption o where
  showOption :: o -> String
  showOptionDescription :: o -> Maybe String

instance IsOption [Char] where
  showOption = id
  showOptionDescription = const Nothing

data Option o = Option
  { oDisplayName :: !String,
    oDescription :: !(Maybe String),
    oValue :: !o
  }

instance IsOption (Option o) where
  showOption = oDisplayName
  showOptionDescription = oDescription

data InteractiveTerminal = InteractiveTerminal
  { inputHandle :: Handle,
    outputHandle :: Handle
  }

askForRequiredInput :: InteractiveTerminal -> String -> IO String
askForRequiredInput terminal question = repeatIfNull terminal $ askForInput terminal question

-- | Asks a yes/no question. Only "y" or "yes" (case-insensitive) count as yes.
askForConfirmation :: InteractiveTerminal -> String -> IO Bool
askForConfirmation terminal question = do
  answer <- askForInput terminal (question <> " [y/N]")
  return $ map toLower answer `elem` ["y", "yes"]

askToChoose :: forall o. (IsOption o) => InteractiveTerminal -> String -> NonEmpty o -> IO o
askToChoose _ _ (singleOption :| []) = return singleOption
askToChoose terminal question options = do
  hPutStrLn terminal.outputHandle $ Term.applyStyles [Term.Bold] question
  hPutStrLn terminal.outputHandle showIndexedOptions
  answer <- prompt terminal
  getOptionMatchingAnswer answer & maybe printErrorAndAskAgain return
  where
    getOptionMatchingAnswer :: String -> Maybe o
    getOptionMatchingAnswer "" = Just defaultOption
    getOptionMatchingAnswer answer =
      getOptionByIndex answer <|> getOptionByName answer

    getOptionByIndex :: String -> Maybe o
    getOptionByIndex idxStr =
      case readMaybe idxStr of
        Just idx | idx >= 1 && idx <= length options -> Just $ options NE.!! (idx - 1)
        _invalidIndex -> Nothing

    getOptionByName :: String -> Maybe o
    getOptionByName name = find ((== name) . showOption) options

    printErrorAndAskAgain :: IO o
    printErrorAndAskAgain = do
      hPutStrLn terminal.outputHandle $ Term.applyStyles [Term.Red] "Invalid selection. Type the name or the index of the option."
      askToChoose terminal question options

    showIndexedOptions :: String
    showIndexedOptions = intercalate "\n" $ showIndexedOption <$> zip [1 ..] (NE.toList options)
      where
        showIndexedOption (idx, option) =
          concat
            [ indexPrefix,
              optionName,
              tags,
              optionDescription
            ]
          where
            indexPrefix = Term.applyStyles [Term.Yellow] (showIndex idx) <> " "
            optionName = Term.applyStyles [Term.Bold] (showOption option)
            tags = whenDefault (Term.applyStyles [Term.Yellow] " (default)")
            optionDescription = showDescription (idx, option)
            whenDefault xs = if isDefaultOption option then xs else mempty

        showIndex idx = "[" ++ show (idx :: Int) ++ "]"

        showDescription (idx, option) = case showOptionDescription option of
          Just description -> "\n" <> replicate indentLength ' ' <> description
          Nothing -> ""
          where
            indentLength = length (showIndex idx) + 1

    defaultOption :: o
    defaultOption = NE.head options

    isDefaultOption :: o -> Bool
    isDefaultOption option = showOption option == showOption defaultOption

askForInput :: InteractiveTerminal -> String -> IO String
askForInput terminal question = do
  hPutStr terminal.outputHandle $ Term.applyStyles [Term.Bold] question
  prompt terminal

tryGettingConfirmation :: InteractiveTerminal -> String -> String -> IO (Either ConfirmationError ())
tryGettingConfirmation terminal message requiredAnswer =
  askForInput terminal message <&> \actualAnswer ->
    if actualAnswer == requiredAnswer
      then Right ()
      else Left $ WrongAnswer actualAnswer

tryGettingConfirmationWithTimeout :: InteractiveTerminal -> String -> String -> Int -> IO (Either ConfirmationError ())
tryGettingConfirmationWithTimeout terminal message requiredAnswer timeoutSeconds =
  timeout timeoutMicroseconds (tryGettingConfirmation terminal message requiredAnswer)
    <&> fromMaybe (Left Timeout)
  where
    timeoutMicroseconds = timeoutSeconds * 10 ^ (6 :: Int)

data ConfirmationError = Timeout | WrongAnswer String

repeatIfNull :: (Foldable t) => InteractiveTerminal -> IO (t a) -> IO (t a)
repeatIfNull terminal action = repeatUntil terminal null "This field cannot be empty." action

repeatUntil :: InteractiveTerminal -> (a -> Bool) -> String -> IO a -> IO a
repeatUntil terminal predicate errorMessage action = do
  result <- action
  if predicate result
    then do
      hPutStrLn terminal.outputHandle $ Term.applyStyles [Term.Red] errorMessage
      repeatUntil terminal predicate errorMessage action
    else return result

-- | Prompts for the user's answer and reads it.. Reaching the end of input
-- while waiting for an answer (Ctrl+D) throws an error 'isPromptInputEOF'
-- recognizes.
prompt :: InteractiveTerminal -> IO String
prompt terminal = do
  putStrFlush terminal $ Term.applyStyles [Term.Yellow] " ▸ "
  T.unpack . T.strip . T.pack <$> hGetLine terminal.inputHandle

-- Explicit flush ensures prompt messages are printed immediately on all systems.
putStrFlush :: InteractiveTerminal -> String -> IO ()
putStrFlush terminal msg = do
  hPutStr terminal.outputHandle msg
  hFlush terminal.outputHandle

stdioTerminal :: InteractiveTerminal
stdioTerminal = InteractiveTerminal {inputHandle = stdin, outputHandle = stdout}
