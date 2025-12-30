module Wasp.Cli.Command.News
  ( news,
    fetchAndReportMustSeeNewsIfDue,
  )
where

import Control.Monad (unless)
import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (isJust)
import System.Environment (lookupEnv)
import Wasp.Cli.Command (Command, CommandError (..))
import Wasp.Cli.Command.News.Action
  ( executeNewsAction,
    makeUserInvokedNewsAction,
    makeWaspInvokedNewsAction,
    shouldWaspInvokeNews,
  )
import Wasp.Cli.Command.News.Fetching (fetchNews, fetchNewsWithTimeout)
import Wasp.Cli.Command.News.LocalNewsState (loadLocalNewsState)
import Wasp.Util (checkIfOnCi, whenM)

news :: Command ()
news = do
  newsEntries <- either (throwError . CommandError "Getting Wasp news failed") pure =<< liftIO fetchNews
  liftIO $ do
    localNewsState <- loadLocalNewsState
    executeNewsAction localNewsState $ makeUserInvokedNewsAction newsEntries

fetchAndReportMustSeeNewsIfDue :: IO ()
fetchAndReportMustSeeNewsIfDue = do
  isWaspNewsDisabled <- isJust <$> lookupEnv "WASP_AUTO_NEWS_DISABLE"
  isOnCi <- checkIfOnCi
  unless (isWaspNewsDisabled || isOnCi) $ do
    localNewsState <- loadLocalNewsState
    whenM (shouldWaspInvokeNews localNewsState) $ do
      fetchNewsWithTimeout 2 >>= \case
        Left _err -> return () -- Wasp stays silent on purpose
        Right newsEntries ->
          executeNewsAction localNewsState $
            makeWaspInvokedNewsAction localNewsState newsEntries
