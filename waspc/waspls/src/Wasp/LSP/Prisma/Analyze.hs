module Wasp.LSP.Prisma.Analyze where

import Control.Lens ((.~))
import Control.Monad.Cont (liftIO)
import Control.Monad.Log.Class (logM)
import StrongPath (Abs, Dir, Path')
import Wasp.LSP.Prisma.Util (showEntities)
import Wasp.LSP.ServerMonads (ServerM, modify)
import qualified Wasp.LSP.ServerState as State
import Wasp.Project (WaspProjectDir)
import Wasp.Project.Analyze (analyzePrismaSchema)

analyzePrismaSchemaFileAndSetEntities :: Path' Abs (Dir WaspProjectDir) -> ServerM ()
analyzePrismaSchemaFileAndSetEntities waspDir = do
  liftIO (analyzePrismaSchema waspDir)
    >>= \case
      Left err -> logM $ "[analyzePrismaSchemaFileAndSetEntities] Error analyzing Prisma schema: " ++ show err
      Right (_, entities) -> do
        logM $ "[analyzePrismaSchemaFileAndSetEntities] Analyzed Prisma schema: " ++ showEntities entities
        modify (State.prismaEntities .~ entities)
