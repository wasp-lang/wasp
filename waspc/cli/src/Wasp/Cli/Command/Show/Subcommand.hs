module Wasp.Cli.Command.Show.Subcommand
  ( ShowSubcommand (..),
    runShowSubcommand,
  )
where

import Control.Monad.IO.Class (liftIO)
import qualified Data.Aeson as Aeson
import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Lazy.Char8 as BSL8
import Wasp.AppSpec.Core.Inspectable (InspectionEntry)
import Wasp.Cli.Command (Command)
import Wasp.Cli.Command.Show.Table (renderEntriesAsTables)

-- | Describes a `wasp show` subcommand: both its CLI parser and its
-- implementation are derived from this description.
data ShowSubcommand = ShowSubcommand
  { name :: String,
    description :: String,
    -- | Help text for the subcommand's --json flag.
    jsonHelp :: String,
    -- | Gathers the data behind the subcommand's human-readable overview.
    getInspectionEntries :: Command [InspectionEntry],
    -- | Gathers the data behind the subcommand's --json output.
    getJson :: Command Aeson.Value
  }

runShowSubcommand :: ShowSubcommand -> Bool -> Command ()
runShowSubcommand subcommand isJson = liftIO . putStr =<< render
  where
    render
      | isJson = renderJson <$> subcommand.getJson
      | otherwise = renderEntriesAsTables <$> subcommand.getInspectionEntries

    renderJson value = BSL8.unpack $ encodePretty value <> "\n"
