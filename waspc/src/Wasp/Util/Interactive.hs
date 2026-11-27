{-# LANGUAGE FlexibleInstances #-}

module Wasp.Util.Interactive
  ( InteractiveTerminal (..),
    askForInput,
    askToChoose,
    askForRequiredInput,
    askForConfirmation,
    IsOption (..),
    Option (..),
  )
where

import Control.Applicative ((<|>))
import Data.Char (toLower)
import Data.Foldable (find)
import Data.Function ((&))
import Data.List (intercalate)
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import Text.Read (readMaybe)
import qualified Wasp.Util.TextStyle as TextStyle

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

-- | What the questions below need from the terminal.
data InteractiveTerminal = InteractiveTerminal
  { write :: String -> IO (),
    -- | Reaching the end of the input while waiting for an answer (Ctrl+D)
    -- throws an error.
    readLine :: IO String
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
  terminal.write $ TextStyle.applyStyles [TextStyle.Bold] question <> "\n"
  terminal.write $ showIndexedOptions <> "\n"
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
      terminal.write $ TextStyle.applyStyles [TextStyle.Red] "Invalid selection. Type the name or the index of the option." <> "\n"
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
            indexPrefix = TextStyle.applyStyles [TextStyle.Yellow] (showIndex idx) <> " "
            optionName = TextStyle.applyStyles [TextStyle.Bold] (showOption option)
            tags = whenDefault (TextStyle.applyStyles [TextStyle.Yellow] " (default)")
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
  terminal.write $ TextStyle.applyStyles [TextStyle.Bold] question
  prompt terminal

repeatIfNull :: (Foldable t) => InteractiveTerminal -> IO (t a) -> IO (t a)
repeatIfNull terminal action = repeatUntil terminal null "This field cannot be empty." action

repeatUntil :: InteractiveTerminal -> (a -> Bool) -> String -> IO a -> IO a
repeatUntil terminal predicate errorMessage action = do
  result <- action
  if predicate result
    then do
      terminal.write $ TextStyle.applyStyles [TextStyle.Red] errorMessage <> "\n"
      repeatUntil terminal predicate errorMessage action
    else return result

-- | Prompts for the user's answer and reads it.
prompt :: InteractiveTerminal -> IO String
prompt terminal = do
  terminal.write $ TextStyle.applyStyles [TextStyle.Yellow] " ▸ "
  T.unpack . T.strip . T.pack <$> terminal.readLine
