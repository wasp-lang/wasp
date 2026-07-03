module Wasp.Project.WaspFile
  ( findWaspFile,
    analyzeWaspFile,
  )
where

import StrongPath
  ( Abs,
    Dir,
    File,
    Path',
  )
import qualified Wasp.AppSpec as AS
import Wasp.AppSpec.Core.Decl.JSON ()
import Wasp.CompileOptions (CompileOptions)
import Wasp.Project.Common
  ( CompileError,
    WaspProjectDir,
    WaspTsFile,
    findFileInWaspProjectDir,
    mainWaspTsFileInWaspProjectDir,
    mainWaspTsxFileInWaspProjectDir,
  )
import Wasp.Project.WaspFile.TypeScript (analyzeWaspTsFile)
import qualified Wasp.Psl.Ast.Schema as Psl.Schema
import qualified Wasp.Util.IO as IOUtil
import Wasp.Util.StrongPath (findAllFilesWithSuffix)

findWaspFile :: Path' Abs (Dir WaspProjectDir) -> IO (Either String (Path' Abs (File WaspTsFile)))
findWaspFile projectDir = do
  hasWaspLang <- hasWaspLangFile projectDir
  mainWaspTsFile <- findFileInWaspProjectDir projectDir mainWaspTsFileInWaspProjectDir
  mainWaspTsxFile <- findFileInWaspProjectDir projectDir mainWaspTsxFileInWaspProjectDir
  return $ case (hasWaspLang, mainWaspTsFile, mainWaspTsxFile) of
    (True, _, _) -> Left dslNoLongerSupportedMessage
    (False, Just _, Just _) -> Left bothFilesFoundMessage
    (False, Just waspTsFile, Nothing) -> Right waspTsFile
    (False, Nothing, Just waspTsxFile) -> Right waspTsxFile
    (False, Nothing, Nothing) -> Left fileNotFoundMessage
  where
    fileNotFoundMessage = "Couldn't find the `main.wasp.ts` (or `main.wasp.tsx`) file in the project directory."
    bothFilesFoundMessage =
      "Found both `main.wasp.ts` and `main.wasp.tsx` in the project directory. "
        ++ "Please define your app in only one of them."
    dslNoLongerSupportedMessage =
      "Defining your app with the Wasp DSL (`main.wasp`) is no longer supported. "
        ++ "Please define your app in TypeScript using Wasp Spec (`main.wasp.ts` or `main.wasp.tsx`). "
        ++ "See https://wasp.sh/docs/general/spec for more details."

hasWaspLangFile :: Path' Abs (Dir WaspProjectDir) -> IO Bool
hasWaspLangFile projectDir = do
  (filesInProjectDir, _) <- IOUtil.listDirectory projectDir
  return $ not . null $ findAllFilesWithSuffix ".wasp" filesInProjectDir

analyzeWaspFile ::
  CompileOptions ->
  Psl.Schema.Schema ->
  Path' Abs (File WaspTsFile) ->
  IO (Either [CompileError] [AS.Decl])
analyzeWaspFile = analyzeWaspTsFile
