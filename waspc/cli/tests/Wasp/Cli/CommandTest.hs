module Wasp.Cli.CommandTest where

import Control.Exception (try)
import Control.Monad.Error.Class (throwError)
import Control.Monad.IO.Class (liftIO)
import Data.IORef (IORef, modifyIORef, newIORef, readIORef)
import System.Exit (ExitCode (ExitFailure))
import Test.Hspec
import Wasp.Cli.Command (Command, CommandError (..), defer, runCommand)

spec_defer :: Spec
spec_defer = do
  describe "defer" $ do
    it "runs deferred cleanups only after the command finishes" $ do
      markers <- newIORef []
      runCommand $ do
        defer $ addMarker markers "cleanup"
        liftIO $ addMarker markers "body"
      readIORef markers `shouldReturn` ["body", "cleanup"]

    it "runs deferred cleanups in reverse order of registration" $ do
      markers <- newIORef []
      runCommand $ do
        defer $ addMarker markers "cleanup 1"
        defer $ addMarker markers "cleanup 2"
        defer $ addMarker markers "cleanup 3"
      readIORef markers `shouldReturn` ["cleanup 3", "cleanup 2", "cleanup 1"]

    it "runs only the cleanups registered before a command error" $ do
      markers <- newIORef []
      _ <- tryRunCommand $ do
        defer $ addMarker markers "cleanup before error"
        _ <- throwError $ CommandError "Test error" "Error message"
        defer $ addMarker markers "cleanup after error"
      readIORef markers `shouldReturn` ["cleanup before error"]

    it "runs cleanups before exiting with failure on a command error" $ do
      markers <- newIORef []
      result <- tryRunCommand $ do
        defer $ addMarker markers "cleanup"
        throwError $ CommandError "Test error" "Error message"
      readIORef markers `shouldReturn` ["cleanup"]
      result `shouldBe` Left (ExitFailure 1)
  where
    addMarker :: IORef [String] -> String -> IO ()
    addMarker markers marker = modifyIORef markers (++ [marker])

    -- On error, 'runCommand' exits the process, which we catch as an
    -- 'ExitCode' exception to keep the test process alive.
    tryRunCommand :: Command () -> IO (Either ExitCode ())
    tryRunCommand = try . runCommand
