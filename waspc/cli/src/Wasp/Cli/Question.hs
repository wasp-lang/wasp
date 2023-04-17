module Wasp.Cli.Question
  ( query,
    queryWithOptions,
    queryNotNull,
  )
where

import Data.List (intercalate)
import qualified Data.Text as T
import System.IO (hFlush, stdout)
import Text.Read (readMaybe)
import qualified Wasp.Util.Terminal as Term

queryNotNull :: String -> IO String
queryNotNull question = do
  ensureNotNull . query $ question

queryWithOptions :: String -> [String] -> IO (Maybe String)
queryWithOptions question options = do
  putStrLn optionsStr
  putStr $ question ++ " "
  answer <- prompt

  handleEmptyAnswer answer
    >>= \case
      Just _ ->
        handleOptionIndex answer
          >>= \case
            Just option -> return $ Just option
            Nothing -> handleOptionName answer
      -- Optional
      Nothing -> return Nothing
  where
    handleEmptyAnswer :: String -> IO (Maybe String)
    handleEmptyAnswer answer = do
      return $ if null answer then Nothing else Just answer

    handleOptionIndex :: String -> IO (Maybe String)
    handleOptionIndex answer = do
      let index = readMaybe answer :: Maybe Int

      case index of
        Just i -> do
          if i `elem` indexes
            then return $ Just $ options !! (i - 1)
            else invalidOptionAction
        Nothing -> return Nothing

    handleOptionName :: String -> IO (Maybe String)
    handleOptionName answer = do
      if answer `elem` options
        then return $ Just answer
        else invalidOptionAction

    invalidOptionAction :: IO (Maybe String)
    invalidOptionAction = do
      putStrLn $ Term.applyStyles [Term.Red] "Invalid selection, write the name or the index of the option."
      queryWithOptions question options

    optionsStr = intercalate "\n" $ showOptionsWithIndexes options
    showOptionsWithIndexes = zipWith (\i o -> indexWithStyle i ++ o) indexes
    indexWithStyle i = Term.applyStyles [Term.Yellow] $ "(" ++ show (i :: Int) ++ ") "
    indexes = [1 .. length options]

query :: String -> IO String
query question = putStr question >> prompt

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

-- | Explicit flush ensures prompt messages are in the correct order on all systems.
putStrFlush :: String -> IO ()
putStrFlush msg = do
  putStr msg
  hFlush stdout
