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
import Wasp.Util.Interactive (IsOption (..), Option (..))

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
