{-# LANGUAGE FlexibleInstances #-}

module Wasp.Cli.Interactive
  ( askForInput,
    askToChoose,
    askForRequiredInput,
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

  We introduced the Option class to get different "show" behavior for Strings and other
  types. If we are using something other then String, an instance of Option needs to be defined,
  but for Strings it just returns the String itself.
-}
class Option o where
  showOption :: o -> String
  showOptionDescription :: o -> Maybe String

instance Option [Char] where
  showOption = id
  showOptionDescription = const Nothing

askForRequiredInput :: String -> IO String
askForRequiredInput = repeatIfNull . askForInput

askToChoose :: forall o. Option o => String -> NonEmpty o -> IO o
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
          Term.applyStyles [Term.Yellow] indexPrefix
            <> Term.applyStyles [Term.Bold] (showOption option)
            <> (if isDefaultOption option then " (default)" else "")
            <> showDescription option (length indexPrefix)
          where
            indexPrefix = showIndex idx <> " "

        showIndex i = "[" ++ show (i :: Int) ++ "]"

        showDescription option indentLength = case showOptionDescription option of
          Just description -> "\n" <> replicate indentLength ' ' <> description
          Nothing -> ""

    defaultOption :: o
    defaultOption = NE.head options

    isDefaultOption :: o -> Bool
    isDefaultOption option = showOption option == showOption defaultOption

askForInput :: String -> IO String
askForInput question = putStr (Term.applyStyles [Term.Bold] question) >> prompt

repeatIfNull :: Foldable t => IO (t a) -> IO (t a)
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
