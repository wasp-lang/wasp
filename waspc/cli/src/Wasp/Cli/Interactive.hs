{-# LANGUAGE FlexibleInstances #-}

module Wasp.Cli.Interactive
  ( askForInput,
    askToChoose,
    askForRequiredInput,
    askForConfirmation,
    tryGettingConfirmationWithTimeout,
    IsOption (..),
    ConfirmationError (..),
    Option (..),
    NonInteractiveHint (..),
    PromptError (..),
  )
where

import Control.Applicative ((<|>))
import qualified Control.Exception as E
import Control.Monad (guard, unless)
import Data.Char (toLower)
import Data.Foldable (find)
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.List (intercalate)
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import System.IO (hFlush, hIsTerminalDevice, stdin, stdout)
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

-- | Tells the user how to run the command without prompts: its usage, with
-- every argument and flag it needs. Shown when we can't prompt because stdin
-- is not an interactive terminal. Every prompt must have one, since we never
-- want to require interactive input: https://clig.dev/#interactivity
--
-- Build it from the command's argument parser (see
-- 'Wasp.Cli.Util.Parser.getParserHelpMessage') so it can't drift from `--help`.
newtype NonInteractiveHint = NonInteractiveHint String

-- | Thrown when a prompt can't get an answer from the user.
data PromptError
  = -- | Stdin is not an interactive terminal (e.g. it is a pipe, a file or
    -- /dev/null), so we didn't prompt. Carries the command's 'NonInteractiveHint'.
    StdinNotInteractive String
  | -- | The user ended the input (Ctrl+D) while we were waiting for an answer.
    PromptCancelled
  deriving (Show)

instance E.Exception PromptError

askForRequiredInput :: String -> NonInteractiveHint -> IO String
askForRequiredInput question hint = repeatIfNull $ askForInput question hint

-- | Asks a yes/no question. Only "y" or "yes" (case-insensitive) count as yes.
askForConfirmation :: String -> NonInteractiveHint -> IO Bool
askForConfirmation question hint = do
  answer <- askForInput (question <> " [y/N]") hint
  return $ map toLower answer `elem` ["y", "yes"]

askToChoose :: forall o. (IsOption o) => String -> NonInteractiveHint -> NonEmpty o -> IO o
askToChoose _ _ (singleOption :| []) = return singleOption
askToChoose question hint options = do
  ensureStdinIsInteractive hint
  putStrLn $ Term.applyStyles [Term.Bold] question
  putStrLn showIndexedOptions
  answer <- prompt
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
      putStrLn $ Term.applyStyles [Term.Red] "Invalid selection. Type the name or the index of the option."
      askToChoose question hint options

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

askForInput :: String -> NonInteractiveHint -> IO String
askForInput question hint = do
  ensureStdinIsInteractive hint
  putStr $ Term.applyStyles [Term.Bold] question
  prompt

tryGettingConfirmationWithTimeout :: String -> String -> Int -> IO (Either ConfirmationError ())
tryGettingConfirmationWithTimeout message requiredAnswer timeoutSeconds =
  timeout timeoutMicroseconds (E.try $ askForInput message hint)
    <&> \case
      Nothing -> Left Timeout
      Just (Left (StdinNotInteractive _)) -> Left NonInteractiveShell
      Just (Left PromptCancelled) -> Left Cancelled
      Just (Right actualAnswer)
        | actualAnswer == requiredAnswer -> Right ()
        | otherwise -> Left $ WrongAnswer actualAnswer
  where
    timeoutMicroseconds = timeoutSeconds * 10 ^ (6 :: Int)
    -- Never shown: the caller handles 'NonInteractiveShell' itself.
    hint = NonInteractiveHint "Confirmation can't be given non-interactively."

data ConfirmationError = Timeout | NonInteractiveShell | Cancelled | WrongAnswer String

repeatIfNull :: (Foldable t) => IO (t a) -> IO (t a)
repeatIfNull action = repeatUntil null "This field cannot be empty." action

repeatUntil :: (a -> Bool) -> String -> IO a -> IO a
repeatUntil predicate errorMessage action = do
  result <- action
  if predicate result
    then do
      putStrLn $ Term.applyStyles [Term.Red] errorMessage
      repeatUntil predicate errorMessage action
    else return result

-- | We only prompt if stdin is an interactive terminal. Otherwise we throw
-- 'StdinNotInteractive' so the user is told how to pass the answer instead.
-- Must be called before printing anything that belongs to the prompt.
ensureStdinIsInteractive :: NonInteractiveHint -> IO ()
ensureStdinIsInteractive (NonInteractiveHint hint) = do
  isInteractive <- hIsTerminalDevice stdin
  unless isInteractive $ E.throwIO $ StdinNotInteractive hint

-- | Reads the user's answer from stdin. Reaching the end of input while
-- waiting for an answer (Ctrl+D) throws 'PromptCancelled'.
prompt :: IO String
prompt = do
  putStrFlush $ Term.applyStyles [Term.Yellow] " ▸ "
  T.unpack . T.strip . T.pack <$> getLineOrCancel
  where
    getLineOrCancel = E.catchJust (guard . isEOFError) getLine (const $ E.throwIO PromptCancelled)

-- Explicit flush ensures prompt messages are printed immediately on all systems.
putStrFlush :: String -> IO ()
putStrFlush msg = do
  putStr msg
  hFlush stdout
