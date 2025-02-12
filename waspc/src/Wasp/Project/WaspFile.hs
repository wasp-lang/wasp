module Wasp.Project.WaspFile
  ( findWaspFile,
    analyzeWaspFile,
  )
where

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
import Wasp.Project.Common
  ( CompileError,
    WaspFilePath (..),
    WaspProjectDir,
  )
import Wasp.Project.WaspFile.TypeScript (analyzeWaspTsFile)
import Wasp.Project.WaspFile.WaspLang (analyzeWaspLangFile)
import qualified Wasp.Psl.Ast.Schema as Psl.Schema
import qualified Wasp.Util.IO as IOUtil
import Wasp.Util.StrongPath (findAllFilesWithSuffix, getFilename)

findWaspFile :: Path' Abs (Dir WaspProjectDir) -> IO (Either String WaspFilePath)
findWaspFile projectDir = do
  filesInProjectDir <- fst <$> IOUtil.listDirectory projectDir
  let filesEndingWithWasp = findAllFilesWithSuffix ".wasp" filesInProjectDir
      filesEndingWithWaspTs = findAllFilesWithSuffix ".wasp.ts" filesInProjectDir
  return $ case (filesEndingWithWasp, filesEndingWithWaspTs) of
    ([], []) -> Left fileNotFoundMessage
    (_ : _, _ : _) -> Left bothFilesFoundMessage
    ([], waspTsFiles) -> case waspTsFiles of
      [singleWaspTsFile] ->
        if fromRelFile singleWaspTsFile == ".wasp.ts"
          then Left (makeInvalidFileNameMessage ".wasp.ts")
          else Right . WaspTs $ castFile (projectDir </> singleWaspTsFile)
      multipleWaspTsFiles -> Left (makeMultipleFilesMessage "*.wasp.ts" (map getFilename multipleWaspTsFiles))
    (waspLangFiles, []) -> case waspLangFiles of
      [singleWaspLangFile] ->
        if fromRelFile singleWaspLangFile == ".wasp"
          then Left (makeInvalidFileNameMessage ".wasp")
          else Right . WaspLang $ castFile (projectDir </> singleWaspLangFile)
      multipleWaspFiles -> Left (makeMultipleFilesMessage "*.wasp" (map getFilename multipleWaspFiles))
  where
    fileNotFoundMessage = "Couldn't find the *.wasp or a *.wasp.ts file in the project directory."
    bothFilesFoundMessage =
      "Found both *.wasp and *.wasp.ts files in the project directory. "
        ++ "You must choose how you want to define your app (using Wasp or TypeScript) and only keep one of them."
    makeMultipleFilesMessage suffix files = "Found multiple " ++ suffix ++ " files in the project directory: " ++ show files ++ ". Please keep only one."
    makeInvalidFileNameMessage suffix = "Your Wasp file can't be called '" ++ suffix ++ "'. Please rename it to something like [name]" ++ suffix ++ "."

analyzeWaspFile ::
  Path' Abs (Dir WaspProjectDir) ->
  Psl.Schema.Schema ->
  WaspFilePath ->
  IO (Either [CompileError] [AS.Decl])
analyzeWaspFile waspDir prismaSchemaAst = \case
  WaspLang waspFilePath -> analyzeWaspLangFile prismaSchemaAst waspFilePath
  WaspTs waspFilePath -> analyzeWaspTsFile waspDir prismaSchemaAst waspFilePath
