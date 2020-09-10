module Command.Start
    ( start
    ) where

import qualified Path as P
import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import Data.List (find, isSuffixOf)

import CompileOptions (CompileOptions (..))
import StrongPath (Path, Abs, Dir, (</>))
import qualified StrongPath as SP
import qualified Lib
import qualified Util.IO
import Command (Command, CommandError(..))
import Command.Common (findWaspProjectRootFromCwd, waspSays)
import qualified Common


start :: Command ()
start = do
    waspRoot <- findWaspProjectRootFromCwd
    waspFile <- findWaspFile waspRoot
    let outDir = waspRoot </> Common.dotWaspDirInWaspProjectDir </> Common.generatedCodeDirInDotWaspDir
    let options = CompileOptions
            { externalCodeDirPath = waspRoot </> Common.extCodeDirInWaspProjectDir }

    -- TODO: This just compiles once. We need `wasp start` to do much more.
    waspSays "Compiling wasp code..."
    errorOrResult <- liftIO $ Lib.compile waspFile outDir options
    case errorOrResult of
        Left compileError -> throwError $ CommandError $ "Compilation failed: " ++ compileError
        Right () -> waspSays "Code has been successfully compiled.\n"

    -- TODO: Do smart install -> if we need to install stuff, install it.
    --   This should be responsibility of Generator, it should tell us how to install stuff.
    --   But who checks out if stuff needs to be installed at all? That should probably be
    --   Generator again. After installation, it should return some kind of data that describes that installation.
    --   Then, next time, we give it data we have about last installation, and it uses that
    --   to decide if installation needs to happen or not. If it happens, it returnes new data again.
    --   Right now we have setup/installation being called, but it has not support for being "smart" yet.
    waspSays "Setting up generated project..."
    setupResult <- liftIO $ Lib.setup outDir
    case setupResult of
        Left setupError -> throwError $ CommandError $ "Setup failed: " ++ setupError
        Right () -> waspSays "Setup successful.\n"

    -- TODO: Check node version and then run `npm start` on both web and server.
    --   Again, this is something that Generator should be responsible for, since it knows how the code is generated.
    --   It should tell us how to start stuff and we just start it. It should even do composing of the outputs,
    --   since it knows more than us about that.
    waspSays "Starting up generated project..."
    startResult <- liftIO $ Lib.start outDir
    case startResult of
        Left startError -> throwError $ CommandError $ "Start failed: " ++ startError
        Right () -> error "This should never happen, start should never end."

    -- TODO: Listen for changes, if they happen, re-generate the code.
  where
      findWaspFile :: Path Abs (Dir d) -> Command (Path Abs SP.File)
      findWaspFile dir = do
          (files, _) <- liftIO $ Util.IO.listDirectory (SP.toPathAbsDir dir)
          case find isWaspFile files of
              Just file -> return $ dir SP.</> SP.fromPathRelFile file
              Nothing -> throwError $ CommandError "No .wasp file present in the root of Wasp project."

      isWaspFile :: P.Path P.Rel P.File -> Bool
      isWaspFile path = ".wasp" `isSuffixOf` P.toFilePath path
                        && (length (P.toFilePath path) > length (".wasp" :: String))

