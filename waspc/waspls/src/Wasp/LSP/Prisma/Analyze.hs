module Wasp.LSP.Prisma.Analyze where

import Control.Lens ((.~))
import Control.Monad.Cont (liftIO)
import Control.Monad.Log.Class (logM)
import StrongPath (Abs, Dir, Path')
import Wasp.LSP.ServerMonads (ServerM, modify)
import qualified Wasp.LSP.ServerState as State
import Wasp.Project (WaspProjectDir)
import Wasp.Project.Analyze (analyzePrismaSchema)
import qualified Wasp.Psl.Ast.Model as Model
import Wasp.Psl.Ast.Schema (getModels)
import qualified Wasp.Psl.Ast.WithCtx as WithCtx

analyzeAndSetPrismaSchema :: Path' Abs (Dir WaspProjectDir) -> ServerM ()
analyzeAndSetPrismaSchema waspDir = do
  liftIO (analyzePrismaSchema waspDir) >>= \case
    (Left err, warnings) -> do
      logOutput "errors" $ show err
      logOutput "warnings" $ show warnings
    (Right prismaSchemaAst, warnings) -> do
      logOutput "warnings" $ show warnings
      logOutput "models" $ show $ Model.getName . WithCtx.getNode <$> getModels prismaSchemaAst
      modify (State.prismaSchemaAst .~ prismaSchemaAst)
  where
    logOutput :: String -> String -> ServerM ()
    logOutput _ [] = return ()
    logOutput outputName output = logM $ "[analyzeAndSetPrismaSchema] Got the following " ++ outputName ++ ": " ++ output
