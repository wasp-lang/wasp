module ExceptionHandlingTest where

import qualified Control.Concurrent.Async as Async
import qualified Control.Exception as E
import Control.Monad.Trans.Resource (ResourceCleanupException (..))
import System.Exit (ExitCode (..))
import Test.Hspec (Spec, describe, it, shouldBe, shouldReturn)
import Wasp.Cli.ExceptionHandling (formatCleanupException, withExceptionReporting)
import Wasp.Job.Subprocess (ProcessTreeDidNotStop (..))

spec_formatCleanupException :: Spec
spec_formatCleanupException =
  describe "cleanup error output" $ do
    let stopFailure = E.toException ProcessTreeDidNotStop
        stopMessage = "Could not stop all development processes. A child process may still be running."

    it "omits cancellation context when reporting cleanup failures" $
      mapM_
        ( \cancellation ->
            formatCleanupException (ResourceCleanupException (Just cancellation) stopFailure [])
              `shouldBe` stopMessage
        )
        [E.toException Async.AsyncCancelled, E.toException E.ThreadKilled]

    it "preserves original and additional failures through nested cleanup wrappers" $ do
      let original = E.toException $ userError "controller failed"
          additional = E.toException $ userError "pipe close failed"
          nested = E.toException $ ResourceCleanupException Nothing stopFailure []
      formatCleanupException (ResourceCleanupException (Just original) nested [additional])
        `shouldBe` ("user error (controller failed)\n" <> stopMessage <> "\nuser error (pipe close failed)")

spec_withExceptionReporting :: Spec
spec_withExceptionReporting =
  describe "exception reporting" $ do
    it "exits unsuccessfully for known process and cleanup failures" $
      mapM_
        ( \exception -> do
            result <- E.try $ withExceptionReporting $ E.throwIO exception
            result `shouldBe` Left (ExitFailure 1)
        )
        [ E.toException ProcessTreeDidNotStop,
          E.toException $ ResourceCleanupException Nothing (E.toException ProcessTreeDidNotStop) []
        ]

    it "preserves ordinary exit statuses" $ do
      result <- E.try $ withExceptionReporting $ E.throwIO $ ExitFailure 130
      result `shouldBe` Left (ExitFailure 130)

    it "lets cancellation propagate" $ do
      result <- E.try $ withExceptionReporting $ E.throwIO Async.AsyncCancelled
      result `shouldBe` Left Async.AsyncCancelled

    it "lets unrelated IO failures propagate" $ do
      let failure = userError "unrelated failure"
      result <- E.try $ withExceptionReporting $ E.throwIO failure
      result `shouldBe` Left failure

    it "leaves successful actions alone" $
      withExceptionReporting (return ()) `shouldReturn` ()
