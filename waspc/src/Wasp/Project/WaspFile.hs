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
    relfile,
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
  let dotWaspPath = waspDir </> [relfile|.wasp|]
  isFile <- IOUtil.doesFileExist dotWaspPath
  if isFile
    then return $ Left "Your Wasp file can't be called just '.wasp'. Please rename it to [something].wasp."
    else do
      files <- fst <$> IOUtil.listDirectory waspDir
      return $ case (findWaspTsFile files, findWaspLangFile files) of
        (tsFiles, langFiles)
          | not (null tsFiles) && not (null langFiles) -> Left bothFilesFoundMessage
          | null tsFiles && null langFiles -> Left fileNotFoundMessage
          | [waspTsFile] <- tsFiles, null langFiles -> Right waspTsFile
          | null tsFiles, [waspLangFile] <- langFiles -> Right waspLangFile
          | otherwise -> Left multipleFilesFoundMessage
  where
    findWaspTsFile files = WaspTs <$> findFileThatEndsWith ".wasp.ts" files
    findWaspLangFile files = WaspLang <$> findFileThatEndsWith ".wasp" files
    findFileThatEndsWith suffix files = castFile . (waspDir </>) <$> findFilesThatEndWith suffix files
    findFilesThatEndWith suffix files = filter ((suffix `isSuffixOf`) . fromRelFile) files
    fileNotFoundMessage = "Couldn't find the *.wasp or a *.wasp.ts file in the " ++ fromAbsDir waspDir ++ " directory"
    bothFilesFoundMessage =
      "Found both *.wasp and *.wasp.ts files in the project directory. "
        ++ "You must choose how you want to define your app (using Wasp or TypeScript) and only keep one of them."
    multipleFilesFoundMessage = "Found multiple *.wasp or *.wasp.ts files in the project directory. Please keep only one."

analyzeWaspFile ::
  Path' Abs (Dir WaspProjectDir) ->
  Psl.Schema.Schema ->
  WaspFilePath ->
  IO (Either [CompileError] [AS.Decl])
analyzeWaspFile waspDir prismaSchemaAst = \case
  WaspLang waspFilePath -> analyzeWaspLangFile prismaSchemaAst waspFilePath
  WaspTs waspFilePath -> analyzeWaspTsFile waspDir prismaSchemaAst waspFilePath
