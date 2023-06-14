module Wasp.LSP.ExtImport
  ( refreshExportsForFile,
  )
where

import Control.Applicative ((<|>))
import Control.Arrow (Arrow (first))
import Control.Lens ((%~))
import Control.Monad (forM_)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Log.Class (logM)
import qualified Data.HashMap.Strict as M
import Data.Maybe (fromJust)
import qualified Language.LSP.Server as LSP
import qualified Path as P
import qualified StrongPath as SP
import qualified StrongPath.Path as SP
import Wasp.LSP.ServerM (ServerM, modify)
import qualified Wasp.LSP.ServerState as State
import Wasp.Project (WaspProjectDir)
import qualified Wasp.TypeScript as TS

-- | Refresh the export cache for the given JS/TS files. This can take a while:
-- generally half a second to a second. It is recommended that this is run in
-- the reactor thread so it does not block other LSP requests from being
-- responded to.
--
-- TODO(before merge): refactor to multiple files at once!
refreshExportsForFile :: SP.Path' SP.Abs SP.File' -> ServerM ()
refreshExportsForFile file = do
  logM $ "[refreshExportsForFile] refreshing " ++ show file
  LSP.getRootPath >>= \case
    Nothing -> pure ()
    Just projectDirFilepath -> do
      logM $ "[refreshExportsForFile] root path is " ++ projectDirFilepath
      -- NOTE: getRootPath always returns a valid absolute path or 'Nothing'.
      let projectDir = fromJust $ SP.parseAbsDir projectDirFilepath
      let maybeExportRequest = ([SP.fromAbsFile file] `TS.TsExportRequest`) . Just . SP.fromAbsFile <$> tryGetTsconfigForFile projectDir file
      forM_ maybeExportRequest $ \exportRequest -> do
        liftIO (TS.getExportsOfTsFiles [exportRequest]) >>= \case
          Left err -> do
            logM $ "[refreshExportsForFile] ERROR getting exports: " ++ show err
          Right res -> do
            logM $ "[refreshExportsForFile] Successfully got exports: " ++ show res
            updateExportsCache res

-- | Look for the tsconfig file for the specified JS/TS file.
--
-- To do this, it checks if the file is inside src/client or src/server and
-- returns the respective tsconfig path if so (src/client/tsconfig.json or
-- src/server/tsconfig.json).
tryGetTsconfigForFile :: SP.Path' SP.Abs (SP.Dir WaspProjectDir) -> SP.Path' SP.Abs SP.File' -> Maybe (SP.Path' SP.Abs SP.File')
tryGetTsconfigForFile waspRoot file = tsconfigPath [SP.reldir|src/client|] <|> tsconfigPath [SP.reldir|src/server|]
  where
    tsconfigPath :: SP.Path' (SP.Rel WaspProjectDir) SP.Dir' -> Maybe (SP.Path' SP.Abs SP.File')
    tsconfigPath folder =
      let absFolder = waspRoot SP.</> folder
       in if SP.toPathAbsDir absFolder `P.isProperPrefixOf` SP.toPathAbsFile file
            then Just $ absFolder SP.</> [SP.relfile|tsconfig.json|]
            else Nothing

updateExportsCache :: TS.TsExportResponse -> ServerM ()
updateExportsCache (TS.TsExportResponse res) = do
  let newExports = M.fromList $ map (first exportResKeyToPath) $ M.toList res
  _ <- modify $ State.tsExports %~ (newExports `M.union`)
  logM "[refreshExportsForFile] finished refreshing"
  where
    -- 'TS.getExportsOfTsFiles' should only ever put valid paths in the keys of
    -- its response, so we enforce that here.
    exportResKeyToPath key = case SP.parseAbsFile key of
      Just path -> path
      Nothing -> error "updateExportsCache: expected valid path from TS.getExportsOfTsFiles."
