module Wasp.Project.WaspFile
  ( findWaspFile,
    isWaspTsProject,
    analyzeWaspFile,
  )
where

import Data.Functor ((<&>))
import StrongPath
  ( Abs,
    Dir,
    Path',
    castFile,
    (</>),
  )
import qualified Wasp.AppSpec as AS
import Wasp.AppSpec.Core.Decl.JSON ()
import Wasp.CompileOptions (CompileOptions)
import Wasp.Project.Common
  ( CompileError,
    WaspFilePath (..),
    WaspProjectDir,
    mainWaspTsFileInWaspProjectDir,
  )
import Wasp.Project.WaspFile.TypeScript (analyzeWaspTsFile)
import Wasp.Project.WaspFile.WaspLang (analyzeWaspLangFile)
import qualified Wasp.Psl.Ast.Schema as Psl.Schema
import qualified Wasp.Util.IO as IOUtil
import Wasp.Util.StrongPath (findAllFilesWithSuffix)

findWaspFile :: Path' Abs (Dir WaspProjectDir) -> IO (Either String WaspFilePath)
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

findWaspTsFile :: Path' Abs (Dir WaspProjectDir) -> IO (Maybe WaspFilePath)
findWaspTsFile projectDir = do
  let fullPath = projectDir </> mainWaspTsFileInWaspProjectDir
  IOUtil.doesFileExist fullPath <&> \case
    True -> Just $ WaspTs $ castFile fullPath
    False -> Nothing

isWaspTsProject :: Path' Abs (Dir WaspProjectDir) -> IO Bool
isWaspTsProject projectDir =
  findWaspFile projectDir >>= \case
    Right (WaspTs _) -> return True
    _ -> return False

analyzeWaspFile ::
  CompileOptions ->
  Psl.Schema.Schema ->
  WaspFilePath ->
  IO (Either [CompileError] [AS.Decl])
analyzeWaspFile compileOptions prismaSchemaAst = \case
  WaspLang waspFilePath -> analyzeWaspLangFile prismaSchemaAst waspFilePath
  WaspTs waspFilePath -> analyzeWaspTsFile compileOptions prismaSchemaAst waspFilePath
