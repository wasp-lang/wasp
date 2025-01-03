module Wasp.Project.WaspFile.WaspLang
  ( analyzeWaspLangFile,
    analyzeWaspFileContent,
  )
where

import Control.Arrow (left)
import StrongPath
  ( Abs,
    File,
    Path',
  )
import qualified Wasp.Analyzer as Analyzer
import Wasp.Analyzer.Parser.Ctx (Ctx)
import qualified Wasp.AppSpec as AS
import Wasp.AppSpec.Core.Decl.JSON ()
import Wasp.Error (showCompilerErrorForTerminal)
import Wasp.Project.Common
  ( CompileError,
    WaspLangFile,
  )
import qualified Wasp.Psl.Ast.Schema as Psl.Schema
import qualified Wasp.Util.IO as IOUtil

analyzeWaspLangFile :: Psl.Schema.Schema -> Path' Abs (File WaspLangFile) -> IO (Either [CompileError] [AS.Decl])
analyzeWaspLangFile prismaSchemaAst waspFilePath = do
  waspFileContent <- IOUtil.readFile waspFilePath
  left (map $ showCompilerErrorForTerminal (waspFilePath, waspFileContent))
    <$> analyzeWaspFileContent prismaSchemaAst waspFileContent

analyzeWaspFileContent :: Psl.Schema.Schema -> String -> IO (Either [(String, Ctx)] [AS.Decl])
analyzeWaspFileContent prismaSchemaAst =
  return
    . left (map Analyzer.getErrorMessageAndCtx)
    . Analyzer.analyze prismaSchemaAst
