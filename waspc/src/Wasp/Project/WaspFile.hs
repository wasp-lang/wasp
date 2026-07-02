module Wasp.Project.WaspFile
  ( findWaspFile,
    analyzeWaspFile,
  )
where

import Data.Functor ((<&>))
import StrongPath
  ( Abs,
    Dir,
    File,
    Path',
    castFile,
    (</>),
  )
import qualified Wasp.AppSpec as AS
import Wasp.AppSpec.Core.Decl.JSON ()
import Wasp.CompileOptions (CompileOptions)
import Wasp.Project.Common
  ( CompileError,
    WaspProjectDir,
    WaspTsFile,
    mainWaspTsFileInWaspProjectDir,
  )
import Wasp.Project.WaspFile.TypeScript (analyzeWaspTsFile)
import qualified Wasp.Psl.Ast.Schema as Psl.Schema
import qualified Wasp.Util.IO as IOUtil
import Wasp.Util.StrongPath (findAllFilesWithSuffix)

findWaspFile :: Path' Abs (Dir WaspProjectDir) -> IO (Either String (Path' Abs (File WaspTsFile)))
findWaspFile projectDir =
  liftA2 (,) (hasWaspLangFile projectDir) (findWaspTsFile projectDir)
    <&> \case
      (True, _) -> Left dslNoLongerSupportedMessage
      (False, Just waspTsFile) -> Right waspTsFile
      (False, Nothing) -> Left fileNotFoundMessage
  where
    fileNotFoundMessage = "Couldn't find the `main.wasp.ts` file in the project directory."
    dslNoLongerSupportedMessage =
      "Defining your app with the Wasp DSL (`main.wasp`) is no longer supported. "
        ++ "Please define your app in TypeScript using Wasp Spec (`main.wasp.ts`). "
        ++ "See https://wasp.sh/docs/general/spec for more details."

hasWaspLangFile :: Path' Abs (Dir WaspProjectDir) -> IO Bool
hasWaspLangFile projectDir = do
  (filesInProjectDir, _) <- IOUtil.listDirectory projectDir
  return $ not . null $ findAllFilesWithSuffix ".wasp" filesInProjectDir

findWaspTsFile :: Path' Abs (Dir WaspProjectDir) -> IO (Maybe (Path' Abs (File WaspTsFile)))
findWaspTsFile projectDir = do
  let fullPath = projectDir </> mainWaspTsFileInWaspProjectDir
  IOUtil.doesFileExist fullPath <&> \case
    True -> Just $ castFile fullPath
    False -> Nothing

analyzeWaspFile ::
  CompileOptions ->
  Psl.Schema.Schema ->
  Path' Abs (File WaspTsFile) ->
  IO (Either [CompileError] [AS.Decl])
analyzeWaspFile = analyzeWaspTsFile
