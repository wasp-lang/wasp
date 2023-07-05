module Wasp.LSP.ExtImport.ExportsCache
  ( refreshExportsForAllExtImports,
    refreshExportsOfFiles,
    lookupExtImport,
    ExtImportLookupResult (..),
  )
where

import Control.Arrow ((&&&))
import Control.Lens ((%~), (^.))
import Control.Monad (void, (<=<))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Log.Class (logM)
import Control.Monad.Reader.Class (asks)
import qualified Data.HashMap.Strict as M
import Data.List (find)
import Data.Maybe (catMaybes, fromJust, mapMaybe)
import qualified Language.LSP.Server as LSP
import qualified StrongPath as SP
import Wasp.Analyzer.Parser (ExtImportName (ExtImportField, ExtImportModule))
import Wasp.LSP.ExtImport.Path (WaspStyleExtFilePath, absPathToCachePath, cachePathToAbsPath, tryGetTsconfigForAbsPath, waspStylePathToCachePath)
import Wasp.LSP.ExtImport.Syntax (ExtImportNode (einName, einPath), getAllExtImports)
import Wasp.LSP.ServerMonads (HandlerM, ServerM, handler, modify)
import qualified Wasp.LSP.ServerState as State
import qualified Wasp.TypeScript.Inspect.Exports as TS

-- | Based on the files imported in the external imports of the current concrete
-- syntax tree, refreshes the exports of files that are imported.
refreshExportsForAllExtImports :: ServerM ()
refreshExportsForAllExtImports = do
  (src, maybeCst) <- handler $ asks ((^. State.currentWaspSource) &&& (^. State.cst))
  case maybeCst of
    Nothing -> pure ()
    Just syntax -> do
      let allExtImports = getAllExtImports src syntax
      let allCachePaths = mapMaybe (waspStylePathToCachePath <=< einPath) allExtImports
      allTsFiles <- catMaybes <$> mapM cachePathToAbsPath allCachePaths
      refreshExportsOfFiles allTsFiles

-- | Update the exports list of several files. Clears the previous cache for
-- each of the files listed, even if it fails to get a new list of exports.
refreshExportsOfFiles :: [SP.Path' SP.Abs SP.File'] -> ServerM ()
refreshExportsOfFiles files = do
  logM $ "[refreshExportsOfFiles] refreshing export lists for " ++ show files

  cachePaths <- catMaybes <$> mapM absPathToCachePath files

  LSP.getRootPath >>= \case
    Nothing -> pure ()
    Just projectRootDirFilePath -> do
      let projectRootDir = fromJust $ SP.parseAbsDir projectRootDirFilePath
      let exportRequests = mapMaybe (getExportRequestForFile projectRootDir) files
      response <- liftIO (TS.getExportsOfTsFiles exportRequests)
      -- Clear cache for all the files that were requested to be updated. This
      -- takes care of removing deleted files from the cache.
      modify (State.tsExports %~ foldr ((.) . M.delete) id cachePaths)
      case response of
        Left err -> do
          logM $ "[refreshExportsForFile] ERROR getting exports: " ++ show err
        Right res -> do
          updateExportsCache res
  where
    -- Find the tsconfig for a given file and return an export request including
    -- that tsconfig. If a tsconfig can not be found (see 'tryGetTsConfigForAbsPath'
    -- for why that might happen), no export request is no returned.
    getExportRequestForFile projectRootDir file = do
      tsconfigPath <- tryGetTsconfigForAbsPath projectRootDir file
      return $
        TS.TsExportsRequest
          { TS.filepaths = [SP.fromAbsFile file],
            TS.tsconfig = Just $ SP.fromAbsFile tsconfigPath
          }

    -- Replaces entries in the exports cache with the exports lists in the
    -- response.
    updateExportsCache :: TS.TsExportsResponse -> ServerM ()
    updateExportsCache (TS.TsExportsResponse res) = do
      newExports <- M.fromList <$> mapM exportResKeyToCachePath (M.toList res)
      void $ modify $ State.tsExports %~ (newExports `M.union`)

    exportResKeyToCachePath (key, exports) = case SP.parseAbsFile key of
      Nothing -> error "[refreshExportsOfFiles]: TS.getExportsOfTsFiles did not respond with a valid path"
      Just path ->
        absPathToCachePath path >>= \case
          Nothing -> error "[refreshExportsOfFiles]: exports refreshed outside of wasp src/ dir (could not get cache path)"
          Just cachePath -> return (cachePath, exports)

-- | The result of 'lookupExtImport'.
data ExtImportLookupResult
  = -- | There is a syntax error in the ExtImport.
    ImportSyntaxError
  | -- | The imported file exists but is not in cached export list.
    ImportCacheMiss
  | -- | The imported file does not exist.
    ImportedFileDoesNotExist !WaspStyleExtFilePath
  | -- | Imports a symbol that is not exported from the file it imports.
    ImportedSymbolDoesNotExist !ExtImportName !WaspStyleExtFilePath
  | -- | Sucessful lookup: includes the file and exported symbol.
    ImportsSymbol !(SP.Path' SP.Abs SP.File') TS.TsExport
  deriving (Eq, Show)

-- | Find the 'TS.TsExport' for the given external import in the export cache.
lookupExtImport :: ExtImportNode -> HandlerM ExtImportLookupResult
lookupExtImport extImport = case (waspStylePathToCachePath =<< einPath extImport, einPath extImport) of
  (Just cachePath, Just waspStylePath) ->
    asks ((M.!? cachePath) . (^. State.tsExports)) >>= \case
      Nothing -> lookupCacheMiss waspStylePath cachePath
      Just exports -> lookupCacheHit cachePath waspStylePath exports
  _ -> return ImportSyntaxError -- No import path provided or the provided path is invalid.
  where
    lookupCacheMiss waspStylePath cachePath =
      cachePathToAbsPath cachePath >>= \case
        Nothing -> return $ ImportedFileDoesNotExist waspStylePath
        Just _ -> return ImportCacheMiss

    lookupCacheHit cachePath waspStylePath exports = case einName extImport of
      Nothing -> return ImportSyntaxError -- No valid name imported.
      Just name -> do
        case find (isImportedExport name) exports of
          Just export ->
            cachePathToAbsPath cachePath >>= \case
              Nothing -> error "[lookupExtImport]: file does not exist after verifying the import is valid"
              Just tsFile -> return $ ImportsSymbol tsFile export
          Nothing -> return $ ImportedSymbolDoesNotExist name waspStylePath

    -- A predicate to check if a 'TS.TsExport' matches an imported 'ExtImportName'.
    isImportedExport importedName = case importedName of
      ExtImportModule _ -> \case
        TS.DefaultExport _ -> True
        _ -> False
      ExtImportField name -> \case
        TS.NamedExport n _ | n == name -> True
        _ -> False
