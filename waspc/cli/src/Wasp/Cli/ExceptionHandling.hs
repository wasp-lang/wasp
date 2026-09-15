module Wasp.Cli.ExceptionHandling
  ( withExceptionReporting,
    formatCleanupException,
  )
where

import qualified Control.Concurrent.Async as Async
import qualified Control.Exception as E
import Control.Monad.Trans.Resource (ResourceCleanupException (..))
import Data.List (intercalate)
import Data.Maybe (isJust)
import System.Exit (exitFailure)
import Wasp.Cli.Message (cliSendMessage)
import Wasp.Job.Subprocess (ProcessTreeDidNotStop)
import qualified Wasp.Message as Msg

withExceptionReporting :: IO () -> IO ()
withExceptionReporting action =
  action
    `E.catches` [ E.Handler reportInternalError,
                  E.Handler reportProcessStopFailure,
                  E.Handler reportCleanupFailure
                ]
  where
    reportInternalError :: E.ErrorCall -> IO ()
    reportInternalError = reportFailure "Internal Wasp error (bug in the compiler)" . E.displayException

    reportProcessStopFailure :: ProcessTreeDidNotStop -> IO ()
    reportProcessStopFailure = reportFailure "Process cleanup failed" . E.displayException

    reportCleanupFailure :: ResourceCleanupException -> IO ()
    reportCleanupFailure = reportFailure "Resource cleanup failed" . formatCleanupException

    reportFailure title message = do
      cliSendMessage $ Msg.Failure title message
      exitFailure

formatCleanupException :: ResourceCleanupException -> String
formatCleanupException cleanup =
  intercalate "\n" $
    maybe [] describeOriginalException (rceOriginalException cleanup)
      ++ map
        describeException
        (rceFirstCleanupException cleanup : rceOtherCleanupExceptions cleanup)
  where
    describeException exception =
      case E.fromException exception of
        Just nestedCleanup -> formatCleanupException nestedCleanup
        Nothing -> E.displayException exception

    describeOriginalException original
      | isJust (E.fromException original :: Maybe Async.AsyncCancelled) = []
      | Just E.ThreadKilled <- E.fromException original = []
      | otherwise = [describeException original]
