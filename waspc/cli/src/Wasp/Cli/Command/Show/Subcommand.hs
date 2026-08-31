{-# LANGUAGE GADTs #-}

module Wasp.Cli.Command.Show.Subcommand
  ( ShowSubcommand (..),
    runShowSubcommand,
  )
where

import Control.Monad.IO.Class (liftIO)
import Data.Aeson (ToJSON)
import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Lazy.Char8 as BSL8
import Wasp.Cli.Command (Command)
import Wasp.Cli.Command.Show.Table (renderEntriesAsTables)
import Wasp.Inspectable (Inspectable, inspect)

-- | Describes a `wasp show` subcommand: both its CLI parser and its
-- implementation are derived from this description.
data ShowSubcommand where
  ShowSubcommand ::
    (ToJSON a, Inspectable a) =>
    {name :: String, description :: String, getData :: Command a} ->
    ShowSubcommand

runShowSubcommand :: ShowSubcommand -> Bool -> Command ()
runShowSubcommand (ShowSubcommand {getData}) asJson =
  (liftIO . putStr) . render =<< getData
  where
    render
      | asJson = renderJson
      | otherwise = renderEntriesAsTables . inspect

    renderJson value = BSL8.unpack $ encodePretty value <> "\n"
