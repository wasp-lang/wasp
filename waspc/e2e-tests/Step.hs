{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Step
  ( Step,
    StepFailure (..),
    makeStep,
    failStep,
    askStepContext,
    -- | NOTE: 'liftStepIO' is meant for implementing step primitives (e.g. in
    -- "Steps"), not for use in test definitions, which should stay declarative.
    liftStepIO,
    withInnerContext,
    runSteps,
  )
where

import Control.Exception (Exception, Handler (..), IOException, catches, throwIO)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ReaderT, ask, asks, runReaderT)
import StrongPath (Abs, File, Path')
import TestLogger (TestLogger, formatFailureWithLog, logStepHeader, withTestLogger)

-- | One executed action of an e2e test, with access to the test's context
-- (e.g. 'Context.TestContext') and the test's logger. A test case is a list of
-- steps, executed in order; the first failing step aborts the test case.
newtype Step ctx a = Step (ReaderT (StepEnv ctx) IO a)
  deriving (Functor, Applicative, Monad)

data StepEnv ctx = StepEnv
  { stepContext :: ctx,
    stepLogger :: TestLogger
  }

-- | Thrown when a step fails (command exit code, failed assertion, file system
-- error, ...). Carries a human-readable description of the failed step.
data StepFailure = StepFailure
  { stepDescription :: String,
    failureDetails :: String
  }

instance Show StepFailure where
  show failure = "Step `" ++ failure.stepDescription ++ "` failed: " ++ failure.failureDetails

instance Exception StepFailure

-- | Creates a step primitive: logs the step's description as a section header
-- in the test log and then runs the given action.
makeStep :: String -> (TestLogger -> ctx -> IO a) -> Step ctx a
makeStep stepDescription action = Step $ do
  env <- ask
  liftIO $ do
    logStepHeader env.stepLogger stepDescription
    action env.stepLogger env.stepContext

-- | Fails the enclosing step.
failStep :: String -> String -> IO a
failStep stepDescription failureDetails = throwIO $ StepFailure stepDescription failureDetails

askStepContext :: Step ctx ctx
askStepContext = Step $ asks (.stepContext)

liftStepIO :: IO a -> Step ctx a
liftStepIO = Step . liftIO

-- | Runs steps that expect a different (usually narrower) context, e.g. running
-- 'Context.WaspProjectContext' steps from within a 'Context.TestContext' test.
withInnerContext :: innerCtx -> [Step innerCtx ()] -> Step ctx ()
withInnerContext innerContext steps = Step $ do
  env <- ask
  liftIO $ runStepsInEnv (StepEnv innerContext env.stepLogger) steps

-- | Runs the steps of a test in order, collecting their output into the given
-- log file. Returns the formatted failure message of the first failed step, if any.
runSteps :: String -> Path' Abs (File f) -> ctx -> [Step ctx ()] -> IO (Either String ())
runSteps testName logFile context steps =
  withTestLogger logFile testName $ \logger -> do
    (Right <$> runStepsInEnv (StepEnv context logger) steps)
      `catches` [ Handler $ \(failure :: StepFailure) -> Left <$> formatFailureWithLog logger (show failure),
                  Handler $ \(ioException :: IOException) -> Left <$> formatFailureWithLog logger ("IO error: " ++ show ioException)
                ]

runStepsInEnv :: StepEnv ctx -> [Step ctx ()] -> IO ()
runStepsInEnv env steps = let (Step reader) = sequence_ steps in runReaderT reader env
