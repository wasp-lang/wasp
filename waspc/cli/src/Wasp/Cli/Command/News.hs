{-# LANGUAGE NamedFieldPuns #-}

module Wasp.Cli.Command.News
  ( news,
    fetchAndReportMandatoryNews,
    -- Exported for testing
    makeMandatoryNewsReport,
    makeMandatoryNewsReportForExistingUser,
    NewsReport (..),
  )
where

import Control.Monad (unless)
import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (isJust)
import System.Environment (lookupEnv)
import Wasp.Cli.Command (Command, CommandError (..))
import Wasp.Cli.Command.News.Fetching (fetchNews, fetchNewsWithTimeout)
import Wasp.Cli.Command.News.Persistence (areNewsStale, obtainLocalNewsInfo)
import Wasp.Cli.Command.News.Report
  ( NewsReport (..),
    makeMandatoryNewsReport,
    makeMandatoryNewsReportForExistingUser,
    makeVoluntaryNewsReport,
    printNewsReportAndUpdateLocalInfo,
  )
import Wasp.Util (whenM)

{-
  TODO list
  - Check the TODOS from this file.
  - Handle the fetching errors (fromJust, etc.).
  - Handle the level (add filtering on wasp start and emphasize it in output, create types, etc.).
  - Properly type and validate stuff on the server.
  - Decide how to deliver the news on the server.
  - Maybe include the project in the monorepo (might make deployment more difficult).
  - Improve how the news look like in the terminal.
  - Test what happens when we add new news on the server.
  - Figure out how to end tests.
  - Figure out what to do with the versions affected field.
  - Thoroughly review the code (there are probably some hacks left over).
  - In `wasp news` output, mark the unread/new news.
-}

news :: Command ()
news =
  liftIO fetchNews >>= \case
    Left err -> throwError $ CommandError "Wasp news failed" err
    Right newsEntries ->
      liftIO $
        printNewsReportAndUpdateLocalInfo $
          makeVoluntaryNewsReport newsEntries

fetchAndReportMandatoryNews :: IO ()
fetchAndReportMandatoryNews = do
  isWaspNewsDisabled <- isJust <$> lookupEnv "WASP_NEWS_DISABLE"
  unless isWaspNewsDisabled $ do
    localNewsInfo <- obtainLocalNewsInfo
    whenM (areNewsStale localNewsInfo) $ do
      fetchNewsWithTimeout 2 >>= \case
        -- TODO: missing prefix for nicer output.
        Left _err -> putStrLn "Couldn't fetch Wasp news, skipping."
        Right newsEntries -> do
          let newsReport = makeMandatoryNewsReport localNewsInfo newsEntries
          printNewsReportAndUpdateLocalInfo newsReport
