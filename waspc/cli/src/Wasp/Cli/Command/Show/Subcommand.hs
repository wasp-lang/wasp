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
import Wasp.AppSpec.Core.Inspectable (Inspectable, inspect)
import Wasp.Cli.Command (Command)
import Wasp.Cli.Command.Show.Table (renderEntriesAsTables)

-- | Describes a `wasp show` subcommand: both its CLI parser and its
-- implementation are derived from this description.
data ShowSubcommand where
  ShowSubcommand ::
    (ToJSON a, Inspectable a) =>
    {name :: String, description :: String, getData :: Command a} ->
    ShowSubcommand

runShowSubcommand :: ShowSubcommand -> Bool -> Command ()
runShowSubcommand (ShowSubcommand {getData}) asJson =
  liftIO . putStr =<< render
  where
    render
      | asJson = renderJson <$> getData
      | otherwise = renderEntriesAsTables . inspect <$> getData

    renderJson value = BSL8.unpack $ encodePretty value <> "\n"
