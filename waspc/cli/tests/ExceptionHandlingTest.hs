module ExceptionHandlingTest where

import qualified Control.Concurrent.Async as Async
import qualified Control.Exception as E
import qualified StrongPath as SP
import System.Directory (getCurrentDirectory)
import System.Exit (ExitCode (..))
import Test.Hspec (Spec, describe, it, shouldBe, shouldReturn)
import Wasp.Cli.ExceptionHandling (withExceptionReporting)
import Wasp.Job (NpmBinNotFound (..), ProcessDidNotStop (..))

spec_withExceptionReporting :: Spec
spec_withExceptionReporting =
  describe "exception reporting" $ do
    it "exits unsuccessfully when a process doesn't stop" $ do
      result <- E.try $ withExceptionReporting $ E.throwIO ProcessDidNotStop
      result `shouldBe` Left (ExitFailure 1)

    it "exits unsuccessfully when an npm package executable is missing" $ do
      currentDir <- SP.parseAbsDir =<< getCurrentDirectory
      result <- E.try $ withExceptionReporting $ E.throwIO $ NpmBinNotFound "vite" currentDir
      result `shouldBe` Left (ExitFailure 1)

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
