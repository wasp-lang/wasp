module Wasp.Project.WaspFile
  ( findWaspFile,
    analyzeWaspFile,
  )
where

import Data.List (find, isSuffixOf)
import StrongPath
  ( Abs,
    Dir,
    Path',
    castFile,
    fromAbsDir,
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

findWaspFile :: Path' Abs (Dir WaspProjectDir) -> IO (Either String WaspFilePath)
findWaspFile waspDir = do
  files <- fst <$> IOUtil.listDirectory waspDir
  return $ case (findWaspTsFile files, findWaspLangFile files) of
    (Just _, Just _) -> Left bothFilesFoundMessage
    (Nothing, Nothing) -> Left fileNotFoundMessage
    (Just waspTsFile, Nothing) -> Right waspTsFile
    (Nothing, Just waspLangFile) -> Right waspLangFile
  where
    findWaspTsFile files = WaspTs <$> findFileThatEndsWith ".wasp.ts" files
    findWaspLangFile files = WaspLang <$> findFileThatEndsWith ".wasp" files
    findFileThatEndsWith suffix files =
      castFile
        . (waspDir </>)
        <$> find ((suffix `isSuffixOf`) . fromRelFile) files

    fileNotFoundMessage = "Couldn't find a *.wasp or a *.wasp.ts file in directory " ++ fromAbsDir waspDir ++ " ."
    bothFilesFoundMessage =
      "Found both *.wasp and *.wasp.ts files in the project directory. "
        ++ "You must choose how you want to define your app (using Wasp or TypeScript) and only keep one of them."

analyzeWaspFile ::
  Path' Abs (Dir WaspProjectDir) ->
  Psl.Schema.Schema ->
  WaspFilePath ->
  IO (Either [CompileError] [AS.Decl])
analyzeWaspFile waspDir prismaSchemaAst = \case
  WaspLang waspFilePath -> analyzeWaspLangFile prismaSchemaAst waspFilePath
  WaspTs waspFilePath -> analyzeWaspTsFile waspDir prismaSchemaAst waspFilePath
