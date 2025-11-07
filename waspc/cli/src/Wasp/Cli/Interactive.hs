{-# LANGUAGE FlexibleInstances #-}

module Wasp.Cli.Interactive
  ( askForInput,
    askToChoose,
    askToChoose',
    askForRequiredInput,
    IsOption (..),
    Option (..),
  )
where

import Control.Applicative ((<|>))
import Data.Foldable (find)
import Data.Function ((&))
import Data.List (intercalate)
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import System.IO (hFlush, stdout)
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

askForRequiredInput :: String -> IO String
askForRequiredInput = repeatIfNull . askForInput

askToChoose' :: String -> NonEmpty (Option o) -> IO o
askToChoose' question options = oValue <$> askToChoose question options

askToChoose :: forall o. (IsOption o) => String -> NonEmpty o -> IO o
askToChoose _ (singleOption :| []) = return singleOption
askToChoose question options = do
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
      putStrLn $ Term.applyStyles [Term.Red] "Invalid selection, write the name or the index of the option."
      askToChoose question options

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

askForInput :: String -> IO String
askForInput question = putStr (Term.applyStyles [Term.Bold] question) >> prompt

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

prompt :: IO String
prompt = do
  putStrFlush $ Term.applyStyles [Term.Yellow] " ▸ "
  T.unpack . T.strip . T.pack <$> getLine

-- Explicit flush ensures prompt messages are printed immediately on all systems.
putStrFlush :: String -> IO ()
putStrFlush msg = do
  putStr msg
  hFlush stdout
