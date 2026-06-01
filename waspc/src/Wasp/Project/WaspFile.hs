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
    fromRelFile,
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
  liftA2 (,) (findWaspLangFile projectDir) (findWaspTsFile projectDir)
    <&> \case
      (Left err, _) -> Left err
      (Right Nothing, Nothing) -> Left fileNotFoundMessage
      (Right (Just _), Just _) -> Left bothFilesFoundMessage
      (Right (Just waspLangFile), _) -> Right waspLangFile
      (_, Just waspTsFile) -> Right waspTsFile
  where
    fileNotFoundMessage = "Couldn't find the *.wasp or a *.wasp.ts file in the project directory."
    bothFilesFoundMessage =
      "Found both *.wasp and *.wasp.ts files in the project directory. "
        ++ "You must choose how you want to define your app (using Wasp or TypeScript) and only keep one of them."

findWaspLangFile :: Path' Abs (Dir WaspProjectDir) -> IO (Either String (Maybe WaspFilePath))
findWaspLangFile projectDir = do
  (filesInProjectDir, _) <- IOUtil.listDirectory projectDir
  let waspLangFiles = findAllFilesWithSuffix ".wasp" filesInProjectDir
  return $ case waspLangFiles of
    [] -> Right Nothing
    [waspFile] ->
      if fromRelFile waspFile == ".wasp"
        then Left invalidFileNameMessage
        else Right $ Just $ WaspLang $ castFile (projectDir </> waspFile)
    _ -> Left $ makeMultipleFilesMessage $ map fromRelFile waspLangFiles
  where
    invalidFileNameMessage = "Your Wasp file can't be called '.wasp'. Please rename it to something like '[name].wasp'."
    makeMultipleFilesMessage files = "Found multiple *.wasp files in the project directory: " ++ show files ++ ". Please keep only one."

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
