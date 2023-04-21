module Wasp.Cli.Interactive
  ( askForInput,
    askToChoose,
    askForRequiredInput,
  )
where

import Data.List (intercalate)
import qualified Data.Text as T
import System.IO (hFlush, stdout)
import Text.Read (readMaybe)
import qualified Wasp.Util.Terminal as Term

askForRequiredInput :: String -> IO String
askForRequiredInput question = do
  ensureNotNull . askForInput $ question

askToChoose :: String -> [String] -> IO String
askToChoose question options = do
  putStrLn $ Term.applyStyles [Term.Bold] question
  putStrLn optionsToChooseFrom
  answer <- prompt

  handleOptionIndex answer >>= \case
    Just option -> return option
    Nothing -> handleOptionName answer
  where
    handleOptionIndex :: String -> IO (Maybe String)
    handleOptionIndex answer = do
      let index = parseIndexOrDefault answer

      case index of
        Just i -> do
          if i `elem` indexes
            then return $ Just $ options !! (i - 1)
            else Just <$> invalidOptionAction
        Nothing -> return Nothing
      where
        -- The default option is the first one
        parseIndexOrDefault input = if null input then Just 1 else readMaybe input :: Maybe Int

    handleOptionName :: String -> IO String
    handleOptionName answer = do
      if answer `elem` options
        then return answer
        else invalidOptionAction

    invalidOptionAction :: IO String
    invalidOptionAction = do
      putStrLn $ Term.applyStyles [Term.Red] "Invalid selection, write the name or the index of the option."
      askToChoose question options

    optionsToChooseFrom = intercalate "\n" $ prependIndexToOption options
    prependIndexToOption = zipWith (\i o -> showIndexWithStyle i ++ o ++ noteForDefault i) indexes
    noteForDefault i = if i == 1 then " (default)" else ""
    showIndexWithStyle i = Term.applyStyles [Term.Yellow] $ "[" ++ show (i :: Int) ++ "] "
    indexes = [1 .. length options]

askForInput :: String -> IO String
askForInput question = putStr (Term.applyStyles [Term.Bold] question) >> prompt

ensureNotNull :: Foldable t => IO (t a) -> IO (t a)
ensureNotNull action = do
  result <- action
  if null result
    then do
      putStrLn $ Term.applyStyles [Term.Red] "This field cannot be empty."
      ensureNotNull action
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
