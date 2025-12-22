module Wasp.Cli.Command.News
  ( news,
    fetchAndReportMandatoryNews,
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
  ( makeMandatoryNewsReport,
    makeVoluntaryNewsReport,
    printNewsReportAndUpdateLocalInfo,
  )
import Wasp.Util (whenM)

{-
  TODO list
  - Check the TODOS from this file.
  - Properly type and validate stuff on the server.
  - Decide how to deliver the news on the server.
  - Improve how the news look like in the terminal.
  - Test what happens when we add new news on the server.
  - Thoroughly review the code (there are probably some hacks left over).

  Future:
  - Figure out what to do with the versions affected field.
  - Maybe include the project in the monorepo (might make deployment more difficult).
  - Figure out how to end tests.
  - In `wasp news` output, mark the unread/new news
-}

news :: Command ()
news =
  liftIO fetchNews >>= \case
    Left err -> throwError $ CommandError "Wasp news failed" err
    Right newsEntries -> liftIO $ do
      localNewsInfo <- obtainLocalNewsInfo
      printNewsReportAndUpdateLocalInfo localNewsInfo $
        makeVoluntaryNewsReport newsEntries

fetchAndReportMandatoryNews :: IO ()
fetchAndReportMandatoryNews = do
  isWaspNewsDisabled <- isJust <$> lookupEnv "WASP_NEWS_DISABLE"
  unless isWaspNewsDisabled $ do
    localNewsInfo <- obtainLocalNewsInfo
    whenM (areNewsStale localNewsInfo) $ do
      fetchNewsWithTimeout 2 >>= \case
        -- TODO: missing prefix for nicer output. Should we even output anything?
        Left _err -> putStrLn "Couldn't fetch Wasp news, skipping."
        Right newsEntries ->
          printNewsReportAndUpdateLocalInfo localNewsInfo $
            makeMandatoryNewsReport localNewsInfo newsEntries
