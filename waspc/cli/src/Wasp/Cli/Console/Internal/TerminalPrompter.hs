-- | The prompter that asks through the terminal. Only "Wasp.Cli.Console" uses
-- it, when it builds the console.
module Wasp.Cli.Console.Internal.TerminalPrompter
  ( makeTerminalPrompter,
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
import Wasp.Cli.Console.Internal.OutputStyle (OutputStyle (..), renderInOutputStyle)
import qualified Wasp.Cli.Console.Internal.Terminal as Terminal
import Wasp.Cli.Console.Prompter (IsOption (..), Prompter (Prompter))
import qualified Wasp.Cli.Console.Prompter as Prompter
import qualified Wasp.Util.TextStyle as TextStyle

makeTerminalPrompter :: OutputStyle -> Prompter
makeTerminalPrompter outputStyle =
  Prompter
    { Prompter.askForInput = askForInput interactiveTerminal,
      Prompter.askForRequiredInput = askForRequiredInput interactiveTerminal,
      Prompter.askForConfirmation = askForConfirmation interactiveTerminal,
      Prompter.askToChoose = askToChoose interactiveTerminal
    }
  where
    interactiveTerminal =
      InteractiveTerminal
        { write = Terminal.writePrompt . renderInOutputStyle outputStyle,
          readLine = Terminal.readLine
        }

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
      terminal.write $ TextStyle.styleError "Invalid selection. Type the name or the index of the option." <> "\n"
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
            indexPrefix = TextStyle.styleAccent (showIndex idx) <> " "
            optionName = TextStyle.applyStyles [TextStyle.Bold] (showOption option)
            tags = whenDefault (TextStyle.styleAccent " (default)")
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
      terminal.write $ TextStyle.styleError errorMessage <> "\n"
      repeatUntil terminal predicate errorMessage action
    else return result

-- | Prompts for the user's answer and reads it.
prompt :: InteractiveTerminal -> IO String
prompt terminal = do
  terminal.write $ TextStyle.styleAccent " ▸ "
  T.unpack . T.strip . T.pack <$> terminal.readLine
