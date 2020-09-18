module Command.Compile
    ( compileIO
    , compile
    ) where

import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Except     (throwError)
import           Data.List              (find, isSuffixOf)
import qualified Path                   as P

import           Command                  (Command, CommandError (..))
import           Command.Common           (findWaspProjectRootDirFromCwd,
                                           waspSaysC)
import qualified Common
import           CompileOptions         (CompileOptions (..))
import qualified Lib
import           StrongPath             (Abs, Dir, Path, (</>))
import qualified StrongPath             as SP
import qualified Util.IO


compile :: Command ()
compile = do
    waspRoot <- findWaspProjectRootDirFromCwd
    let outDir = waspRoot </> Common.dotWaspDirInWaspProjectDir </> Common.generatedCodeDirInDotWaspDir

    waspSaysC "Compiling wasp code..."
    compilationResult <- liftIO $ compileIO waspRoot outDir
    case compilationResult of
        Left compileError -> throwError $ CommandError $ "Compilation failed: " ++ compileError
        Right () -> waspSaysC "Code has been successfully compiled, project has been generated.\n"

-- | Compiles Wasp source code in waspProjectDir directory and generates a project
--   in given outDir directory.
compileIO :: Path Abs (Dir Common.WaspProjectDir)
        -> Path Abs (Dir Lib.ProjectRootDir)
        -> IO (Either String ())
compileIO waspProjectDir outDir = do
    maybeWaspFile <- findWaspFile waspProjectDir
    case maybeWaspFile of
        Nothing -> return $ Left "No *.wasp file present in the root of Wasp project."
        Just waspFile -> Lib.compile waspFile outDir options
  where
    options = CompileOptions
        { externalCodeDirPath = waspProjectDir </> Common.extCodeDirInWaspProjectDir }

    findWaspFile :: Path Abs (Dir d) -> IO (Maybe (Path Abs SP.File))
    findWaspFile dir = do
        (files, _) <- liftIO $ Util.IO.listDirectory (SP.toPathAbsDir dir)
        return $ (dir SP.</>) . SP.fromPathRelFile <$> find isWaspFile files

    isWaspFile :: P.Path P.Rel P.File -> Bool
    isWaspFile path = ".wasp" `isSuffixOf` P.toFilePath path
                      && (length (P.toFilePath path) > length (".wasp" :: String))
