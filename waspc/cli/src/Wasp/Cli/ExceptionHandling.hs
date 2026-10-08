module Wasp.Cli.ExceptionHandling
  ( withExceptionReporting,
  )
where

import qualified Control.Exception as E
import System.Exit (exitFailure)
import Wasp.Cli.Message (cliSendMessage)
import Wasp.Job (ProcessGroupDidNotStop)
import qualified Wasp.Message as Msg

withExceptionReporting :: IO () -> IO ()
withExceptionReporting action =
  action
    `E.catches` [ E.Handler reportInternalError,
                  E.Handler reportProcessStopFailure
                ]
  where
    reportInternalError :: E.ErrorCall -> IO ()
    reportInternalError = reportFailure "Internal Wasp error (bug in the compiler)" . E.displayException

    reportProcessStopFailure :: ProcessGroupDidNotStop -> IO ()
    reportProcessStopFailure = reportFailure "Process cleanup failed" . E.displayException

    reportFailure title message = do
      cliSendMessage $ Msg.Failure title message
      exitFailure
