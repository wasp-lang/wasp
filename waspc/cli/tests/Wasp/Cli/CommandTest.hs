module Wasp.Cli.CommandTest where

import Control.Concurrent (newEmptyMVar, putMVar, readMVar, tryPutMVar)
import Control.Concurrent.Async (waitCatch, withAsync)
import qualified Control.Exception as E
import Control.Monad (replicateM_, void)
import qualified Control.Monad.Catch as C
import Control.Monad.IO.Class (liftIO)
import Data.IORef (newIORef, readIORef, writeIORef)
import System.Exit (ExitCode (ExitFailure))
import System.Timeout (timeout)
import Test.Hspec
import Wasp.Cli.Command (runCommand)

spec_commandShutdown :: Spec
spec_commandShutdown = do
  it "finishes acquisition and releases the resource without starting work" $ do
    shutdown <- newEmptyMVar
    acquiring <- newEmptyMVar
    acquired <- newEmptyMVar
    released <- newIORef False
    used <- newIORef False
    let command =
          C.bracket
            (liftIO $ putMVar acquiring () >> readMVar acquired)
            (const $ liftIO $ writeIORef released True)
            (const $ liftIO $ writeIORef used True)
    withinDeadline $ withAsync (runCommand shutdown command) $ \worker -> do
      readMVar acquiring
      putMVar shutdown $ ExitFailure 130
      putMVar acquired ()
      result <- waitCatch worker
      result `shouldSatisfy` isShutdown
    readIORef released `shouldReturn` True
    readIORef used `shouldReturn` False

  it "finishes release when shutdown arrives after normal work completes" $ do
    shutdown <- newEmptyMVar
    releasing <- newEmptyMVar
    finishRelease <- newEmptyMVar
    released <- newIORef False
    let release = liftIO $ do
          putMVar releasing ()
          readMVar finishRelease
          writeIORef released True
        command = C.bracket (pure ()) (const release) (const $ pure ())
    withinDeadline $ withAsync (runCommand shutdown command) $ \worker -> do
      readMVar releasing
      replicateM_ 3 $ void $ tryPutMVar shutdown $ ExitFailure 130
      putMVar finishRelease ()
      result <- waitCatch worker
      result `shouldSatisfy` isShutdown
    readIORef released `shouldReturn` True

  it "finishes release after command failure despite shutdown requests" $ do
    shutdown <- newEmptyMVar
    releasing <- newEmptyMVar
    finishRelease <- newEmptyMVar
    released <- newIORef False
    let release = liftIO $ putMVar releasing () >> readMVar finishRelease >> writeIORef released True
        command = C.bracket (pure ()) (const release) (const $ liftIO $ ioError $ userError "command failed")
    withinDeadline $ withAsync (runCommand shutdown command) $ \worker -> do
      readMVar releasing
      replicateM_ 3 $ void $ tryPutMVar shutdown $ ExitFailure 130
      putMVar finishRelease ()
      result <- waitCatch worker
      result `shouldSatisfy` isFailure
    readIORef released `shouldReturn` True

  it "cancels concurrent commands through the same shutdown request" $ do
    shutdown <- newEmptyMVar
    firstStarted <- newEmptyMVar
    secondStarted <- newEmptyMVar
    never <- newEmptyMVar
    let command started = liftIO $ putMVar started () >> readMVar never
    withinDeadline $ withAsync (runCommand shutdown $ command firstStarted) $ \first ->
      withAsync (runCommand shutdown $ command secondStarted) $ \second -> do
        readMVar firstStarted
        readMVar secondStarted
        putMVar shutdown $ ExitFailure 130
        firstResult <- waitCatch first
        secondResult <- waitCatch second
        firstResult `shouldSatisfy` isShutdown
        secondResult `shouldSatisfy` isShutdown

isShutdown :: Either E.SomeException () -> Bool
isShutdown (Left exception) = E.fromException exception == Just (ExitFailure 130)
isShutdown _ = False

isFailure :: Either E.SomeException () -> Bool
isFailure (Left exception) = case E.fromException exception :: Maybe E.IOException of
  Just _ -> True
  Nothing -> False
isFailure _ = False

withinDeadline :: IO () -> IO ()
withinDeadline action = timeout 5000000 action >>= (`shouldBe` Just ())
