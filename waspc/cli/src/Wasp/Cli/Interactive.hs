{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Wasp.Cli.Interactive
  ( askForInput,
    askToChoose,
    askForRequiredInput,
    Option,
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
  when printed. We want to avoid this so users can just type the name of the option
  without having to type the quotes as well.

  We do this by overriding the default Show instance for Strings with our own instance
  that just returns the String itself.
-}
class Option o where
  showOption :: o -> String

instance {-# OVERLAPPING #-} Option [Char] where
  showOption = id

instance {-# OVERLAPPABLE #-} Show t => Option t where
  showOption = show

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
          showIndex idx <> " " <> showOption option <> (if isDefaultOption option then " (default)" else "")
        showIndex i = Term.applyStyles [Term.Yellow] $ "[" ++ show (i :: Int) ++ "]"

    defaultOption :: o
    defaultOption = NE.head options

    isDefaultOption :: o -> Bool
    isDefaultOption option = showOption option == showOption defaultOption

askForInput :: String -> IO String
askForInput question = putStr (Term.applyStyles [Term.Bold] question) >> prompt

repeatIfNull :: Foldable t => IO (t a) -> IO (t a)
repeatIfNull action = repeatUtil null "This field cannot be empty." action

repeatUtil :: (a -> Bool) -> String -> IO a -> IO a
repeatUtil predicate errorMessage action = do
  result <- action
  if predicate result
    then do
      putStrLn $ Term.applyStyles [Term.Red] errorMessage
      repeatUtil predicate errorMessage action
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
