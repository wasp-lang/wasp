{-# LANGUAGE FlexibleInstances #-}

module Wasp.Cli.Interactive
  ( InteractiveTerminal,
    getInteractiveTerminal,
    askForInput,
    askToChoose,
    askForRequiredInput,
    tryGettingConfirmation,
    askForConfirmation,
    tryGettingConfirmationWithTimeout,
    IsOption (..),
    ConfirmationError (..),
    Option (..),
    PromptCancelled (..),
  )
where

import Control.Applicative ((<|>))
import qualified Control.Exception as E
import Control.Monad (guard)
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
import System.IO.Error (isEOFError)
import System.Timeout (timeout)
import Text.Read (readMaybe)
import qualified Wasp.Util.Terminal as Term

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

-- | Proof that stdin is an interactive terminal, so we may prompt the user,
-- together with the handles the prompts talk through. The only way to get
-- one is 'getInteractiveTerminal'. Every prompt takes one, so a command
-- can't prompt without having checked first, and the check happens before
-- anything belonging to the prompt is printed.
--
-- We never want to require interactive input (https://clig.dev/#interactivity),
-- so the caller decides what to do without a terminal: fail with a way to
-- pass the answers as arguments, or skip the question.
data InteractiveTerminal = InteractiveTerminal
  { inputHandle :: Handle,
    outputHandle :: Handle
  }

-- | Returns 'Nothing' when stdin is not an interactive terminal (e.g. it is
-- a pipe, a file or /dev/null).
getInteractiveTerminal :: IO (Maybe InteractiveTerminal)
getInteractiveTerminal = do
  isTerminal <- hIsTerminalDevice stdin
  return $
    if isTerminal
      then Just InteractiveTerminal {inputHandle = stdin, outputHandle = stdout}
      else Nothing

-- | Thrown when the user ends the input (Ctrl+D) while we are waiting for an answer.
data PromptCancelled = PromptCancelled
  deriving (Show)

instance E.Exception PromptCancelled

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
  E.try (askForInput terminal message) <&> \case
    Left PromptCancelled -> Left Cancelled
    Right actualAnswer
      | actualAnswer == requiredAnswer -> Right ()
      | otherwise -> Left $ WrongAnswer actualAnswer

tryGettingConfirmationWithTimeout :: InteractiveTerminal -> String -> String -> Int -> IO (Either ConfirmationError ())
tryGettingConfirmationWithTimeout terminal message requiredAnswer timeoutSeconds =
  timeout timeoutMicroseconds (tryGettingConfirmation terminal message requiredAnswer)
    <&> fromMaybe (Left Timeout)
  where
    timeoutMicroseconds = timeoutSeconds * 10 ^ (6 :: Int)

data ConfirmationError = Timeout | Cancelled | WrongAnswer String

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

-- | Reads the user's answer. Reaching the end of input while waiting for an
-- answer (Ctrl+D) throws 'PromptCancelled'.
prompt :: InteractiveTerminal -> IO String
prompt terminal = do
  hPutStr terminal.outputHandle $ Term.applyStyles [Term.Yellow] " ▸ "
  -- Explicit flush ensures the prompt marker is shown immediately on all systems.
  hFlush terminal.outputHandle
  T.unpack . T.strip . T.pack <$> getLineOrCancel
  where
    getLineOrCancel =
      E.catchJust (guard . isEOFError) (hGetLine terminal.inputHandle) (const $ E.throwIO PromptCancelled)
