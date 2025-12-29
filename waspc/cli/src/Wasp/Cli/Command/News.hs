module Wasp.Cli.Command.News
  ( news,
    fetchAndReportWaspInvokedNewsIfDue,
  )
where

import Control.Monad (unless)
import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (isJust)
import System.Environment (lookupEnv)
import Wasp.Cli.Command (Command, CommandError (..))
import Wasp.Cli.Command.News.Fetching (fetchNews, fetchNewsWithTimeout)
import Wasp.Cli.Command.News.LocalNewsState (areNewsStale, loadLocalNewsState)
import Wasp.Cli.Command.News.Report
  ( executeNewsAction,
    makeUserInvokedNewsAction,
    makeWaspInvokedNewsAction,
  )
import Wasp.Util (checkIfOnCi, whenM)

news :: Command ()
news =
  liftIO fetchNews >>= \case
    Left err -> throwError $ CommandError "Getting Wasp news failed" err
    Right newsEntries -> liftIO $ do
      localNewsState <- loadLocalNewsState
      executeNewsAction localNewsState $ makeUserInvokedNewsAction newsEntries

fetchAndReportWaspInvokedNewsIfDue :: IO ()
fetchAndReportWaspInvokedNewsIfDue = do
  isWaspNewsDisabled <- isJust <$> lookupEnv "WASP_AUTO_NEWS_DISABLE"
  isOnCi <- checkIfOnCi
  unless (isWaspNewsDisabled || isOnCi) $ do
    localNewsState <- loadLocalNewsState
    whenM (areNewsStale localNewsState) $ do
      fetchNewsWithTimeout 2 >>= \case
        Left _err -> return () -- Wasp stays silent on purpose
        Right newsEntries ->
          executeNewsAction localNewsState $
            makeWaspInvokedNewsAction localNewsState newsEntries
