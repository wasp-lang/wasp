-- | The monad that e2e test cases are written in.
module TestAction
  ( TestAction,
    runTestAction,
    logInfo,
  )
where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ReaderT, runReaderT)
import System.IO (hPutStrLn, stderr)

-- | The actions of a single e2e test: 'IO' with read-only access to the test's
-- context (e.g. 'Context.TestContext').
--
-- This is a plain type alias, so a test case is an ordinary imperative @do@
-- block: use 'Control.Monad.Reader.ask' / 'Control.Monad.Reader.asks' to reach
-- the context, and 'Control.Monad.IO.Class.liftIO' for whatever else the test
-- needs. Actions shared by several tests live in "SharedActions"; an action only one
-- test needs should just be written inline in that test.
type TestAction ctx = ReaderT ctx IO

-- | Runs a test's actions in the given context. A failing action throws
-- (e.g. via 'Test.Tasty.HUnit.assertFailure'), which aborts the test.
runTestAction :: ctx -> TestAction ctx a -> IO a
runTestAction context action = runReaderT action context

-- | Prints a progress message, for use directly in a test case body.
--
-- Use it sparingly, to mark a long phase of a slow test. A message per action
-- is noise: test cases run concurrently, so these interleave.
--
-- Goes to stderr to stay out of tasty's progress rendering on stdout.
logInfo :: String -> TestAction ctx ()
logInfo = liftIO . hPutStrLn stderr
