module Command.Start
    ( start
    ) where

import           Control.Concurrent.Async (race)
import           Control.Monad.Except     (throwError)
import           Control.Monad.IO.Class   (liftIO)

import qualified Cli.Common               as Common
import           Command                  (Command, CommandError (..))
import           Command.Common           (findWaspProjectRootDirFromCwd,
                                           waspSaysC)
import           Command.Compile          (compileIO)
import           Command.Watch            (watch)
import qualified Lib
import           StrongPath               ((</>))


-- | Does initial compile of wasp code and then runs the generated project.
-- It also listens for any file changes and recompiles and restarts generated project accordingly.
start :: Command ()
start = do
    waspRoot <- findWaspProjectRootDirFromCwd
    let outDir = waspRoot </> Common.dotWaspDirInWaspProjectDir </> Common.generatedCodeDirInDotWaspDir

    waspSaysC "Compiling wasp code..."
    compilationResult <- liftIO $ compileIO waspRoot outDir
    case compilationResult of
        Left compileError -> throwError $ CommandError $ "Compilation failed: " ++ compileError
        Right () -> waspSaysC "Code has been successfully compiled, project has been generated.\n"

    -- TODO: Do smart install -> if we need to install stuff, install it, otherwise don't.
    --   This should be responsibility of Generator, it should tell us how to install stuff.
    --   But who checks out if stuff needs to be installed at all? That should probably be
    --   Generator again. After installation, it should return some kind of data that describes that installation.
    --   Then, next time, we give it data we have about last installation, and it uses that
    --   to decide if installation needs to happen or not. If it happens, it returnes new data again.
    --   Right now we have setup/installation being called, but it has not support for being "smart" yet.
    waspSaysC "Setting up generated project..."
    setupResult <- liftIO $ Lib.setup outDir
    case setupResult of
        Left setupError -> throwError $ CommandError $ "\nSetup failed: " ++ setupError
        Right () -> waspSaysC "\nSetup successful.\n"

    waspSaysC "\nListening for file changes..."
    waspSaysC "Starting up generated project..."
    watchOrStartResult <- liftIO $ race (watch waspRoot outDir) (Lib.start outDir)
    case watchOrStartResult of
        Left () -> error "This should never happen, listening for file changes should never end but it did."
        Right startResult -> case startResult of
            Left startError -> throwError $ CommandError $ "Start failed: " ++ startError
            Right () -> error "This should never happen, start should never end but it did."
