{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}

module Wasp.Cli.Console.Prompter
  ( Prompter (..),
    tryGettingConfirmation,
    tryGettingConfirmationWithTimeout,
    IsOption (..),
    ConfirmationError (..),
    Option (..),
  )
where

import Data.Functor ((<&>))
import Data.List.NonEmpty (NonEmpty)
import Data.Maybe (fromMaybe)
import System.Timeout (timeout)

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

-- | Asks the user questions and reads the answers. It exists only when there
-- is a user to answer them.
--
-- Reaching the end of the input while waiting for an answer (Ctrl+D) throws an
-- error.
data Prompter = Prompter
  { askForInput :: String -> IO String,
    askForRequiredInput :: String -> IO String,
    -- | Asks a yes/no question.
    askForConfirmation :: String -> IO Bool,
    -- | Record dot syntax doesn't work on this field, so call it as
    -- @askToChoose prompter@.
    askToChoose :: forall o. (IsOption o) => String -> NonEmpty o -> IO o
  }

tryGettingConfirmation :: Prompter -> String -> String -> IO (Either ConfirmationError ())
tryGettingConfirmation prompter message requiredAnswer =
  askForInput prompter message <&> \actualAnswer ->
    if actualAnswer == requiredAnswer
      then Right ()
      else Left $ WrongAnswer actualAnswer

tryGettingConfirmationWithTimeout :: Prompter -> String -> String -> Int -> IO (Either ConfirmationError ())
tryGettingConfirmationWithTimeout prompter message requiredAnswer timeoutSeconds =
  timeout timeoutMicroseconds (tryGettingConfirmation prompter message requiredAnswer)
    <&> fromMaybe (Left Timeout)
  where
    timeoutMicroseconds = timeoutSeconds * 10 ^ (6 :: Int)

data ConfirmationError = Timeout | WrongAnswer String
