{-# LANGUAGE DataKinds #-}

module Wasp.LSP.DynamicHandlers
  ( registerDynamicCapabilities,
  )
where

import Control.Lens ((.~), (^.))
import Control.Monad.Log.Class (logM)
import Control.Monad.Reader.Class (asks)
import Data.List (isSuffixOf)
import Data.Maybe (mapMaybe)
import qualified Data.Text as Text
import qualified Language.LSP.Server as LSP
import qualified Language.LSP.Types as LSP
import qualified Language.LSP.Types.Lens as LSP
import Wasp.LSP.Analysis (publishDiagnostics)
import Wasp.LSP.ExtImport.Diagnostic (updateMissingExtImportDiagnostics)
import Wasp.LSP.ExtImport.ExportsCache (refreshExportsOfFiles)
import qualified Wasp.LSP.Prisma.Analyze as Prisma
import Wasp.LSP.ServerMonads (ServerM, handler, modify, sendToReactor)
import qualified Wasp.LSP.ServerState as State
import Wasp.LSP.Util (getWaspDirFromWaspFileUri, lspUriToPath)

-- | Sends capability registration requests for all dynamic capabilities that
-- @waspls@ uses.
--
-- Dynamic capability registration comes from the LSP specification:
-- https://microsoft.github.io/language-server-protocol/specifications/specification-3-16/#client_registerCapability
--
-- This is different than static registration, which occurs when the server is
-- initialized: statically registered capabilities can never be unregistered and
-- only some capabilities can be statically registered.
--
-- In contrast, dynamic registration allows us to register handlers for any
-- capability, as well as unregister it. Specific to the haskell @lsp@ library
-- we use, we can also track whether the capability was successfully registered
-- only for dynamic capabilities.
--
-- In turn, clients may only support a limited set of these capabilities, so
-- the LSP allows the client to refuse to register some capabilities that waspls
-- tries to register. See each @registerX@ function for details on what
-- functionality is lost if registration for that capability fails.
registerDynamicCapabilities :: ServerM ()
registerDynamicCapabilities =
  sequence_
    [ registerJsTsSourceFileWatcher,
      registerPrismaSchemaFileWatcher
    ]

-- | Register file watcher watcher for JS and TS files in the src/ directory.
-- When these files change, the export lists for the changed files are
-- automatically refreshed.
--
-- Note that this registration is guaranteed: if it fails, places in @waspls@ that
-- rely on up-to-date export lists need to manually refresh the export lists.
registerJsTsSourceFileWatcher :: ServerM ()
registerJsTsSourceFileWatcher = do
  -- We try to watch just the @src/@ directory, but we can only specify absolute
  -- glob patterns. So we can only do this when the root path is available, which
  -- it practically always is after @Initialized@ is sent.
  --
  -- NOTE: relative glob patterns are introduced in the LSP spec 3.17, but are
  -- not available in 3.16. We are limited to 3.16 because we use lsp-1.4.0.0.
  let tsJsGlobPattern = "**/*.{ts,tsx,js,jsx}"
  globPattern <-
    -- NOTE: We use the workspace root, instead of the wasp root here, because
    -- 1) The wasp root may not be known yet.
    -- 2) Using the workspace root results only in the potential to consider
    --    files that do not matter. Important files won't be missed (the workspace
    --    root will necessarily contain the wasp root).
    LSP.getRootPath >>= \case
      Nothing -> do
        logM "Could not access projectRootDir when setting up source file watcher. Watching any TS/JS file instead of limiting to src/."
        return tsJsGlobPattern
      Just projectRootDir -> do
        let srcGlobPattern = "src/" <> tsJsGlobPattern
        return $
          if "/" `isSuffixOf` projectRootDir
            then projectRootDir <> srcGlobPattern
            else projectRootDir <> "/" <> srcGlobPattern

  watchSourceFilesToken <-
    LSP.registerCapability
      LSP.SWorkspaceDidChangeWatchedFiles
      LSP.DidChangeWatchedFilesRegistrationOptions
        { _watchers =
            LSP.List
              [ LSP.FileSystemWatcher
                  { _globPattern = Text.pack globPattern,
                    _kind = Nothing
                  }
              ]
        }
      sourceJsTsFilesChangeHandler
  case watchSourceFilesToken of
    Nothing -> logM "[initializedHandler] Client did not accept WorkspaceDidChangeWatchedFiles registration"
    Just _ -> logM $ "[initializedHandler] WorkspaceDidChangeWatchedFiles registered for JS/TS source files. Glob pattern: " <> globPattern
  modify (State.regTokens . State.watchSourceFilesToken .~ watchSourceFilesToken)

-- | Ran when files in src/ change. It refreshes the relevant export lists in
-- the cache and updates missing import diagnostics.
--
-- Both of these tasks are ran in the reactor thread so that other requests
-- can still be answered.
sourceJsTsFilesChangeHandler :: LSP.Handler ServerM 'LSP.WorkspaceDidChangeWatchedFiles
sourceJsTsFilesChangeHandler msg = do
  let (LSP.List uris) = fmap (^. LSP.uri) $ msg ^. LSP.params . LSP.changes
  logM $ "[watchSourceFilesHandler] Received file changes: " ++ show uris
  let fileUris = mapMaybe lspUriToPath uris
  sendToReactor $ do
    -- Refresh export list for modified files
    refreshExportsOfFiles fileUris
    -- Update diagnostics for the wasp files
    updateMissingExtImportDiagnostics
    handler $
      asks (^. State.waspFileUri) >>= \case
        Just uri -> do
          logM $ "[watchSourceFilesHandler] Updating missing diagnostics for " ++ show fileUris
          publishDiagnostics uri
        Nothing -> pure ()

-- | Register file watcher watcher for Prisma schema file.
registerPrismaSchemaFileWatcher :: ServerM ()
registerPrismaSchemaFileWatcher = do
  let prismaSchemaGlob = "schema.prisma"

  globPattern <-
    -- NOTE: We use the workspace root, instead of the wasp root here, because
    -- 1) The wasp root may not be known yet.
    -- 2) Using the workspace root results only in the potential to consider
    --    files that do not matter. Important files won't be missed (the workspace
    --    root will necessarily contain the wasp root).
    LSP.getRootPath >>= \case
      Nothing -> do
        logM "Could not access projectRootDir when setting up source file watcher. Watching any schema.prisma file instead of limiting to src/."
        return prismaSchemaGlob
      Just projectRootDir -> do
        return $
          if "/" `isSuffixOf` projectRootDir
            then projectRootDir <> prismaSchemaGlob
            else projectRootDir <> "/" <> prismaSchemaGlob

  watchPrismaSchemaToken <-
    LSP.registerCapability
      LSP.SWorkspaceDidChangeWatchedFiles
      LSP.DidChangeWatchedFilesRegistrationOptions
        { _watchers =
            LSP.List
              [ LSP.FileSystemWatcher
                  { _globPattern = Text.pack globPattern,
                    _kind = Nothing
                  }
              ]
        }
      prismaSchemaFileChangeHandler
  case watchPrismaSchemaToken of
    Nothing -> logM "[registerPrismaSchemaFileWatcher] Client did not accept WorkspaceDidChangeWatchedFiles registration"
    Just _ -> logM $ "[registerPrismaSchemaFileWatcher] WorkspaceDidChangeWatchedFiles registered for Prisma schema file. Glob pattern: " <> globPattern
  modify (State.regTokens . State.watchPrismaSchemaToken .~ watchPrismaSchemaToken)

-- | Ran when Prisma schema file changes.
prismaSchemaFileChangeHandler :: LSP.Handler ServerM 'LSP.WorkspaceDidChangeWatchedFiles
prismaSchemaFileChangeHandler msg = do
  let (LSP.List uris) = fmap (^. LSP.uri) $ msg ^. LSP.params . LSP.changes
  logM $ "[prismaSchemaFileChangeHandler] Received file changes: " ++ show uris
  sendToReactor $ do
    case getWaspDirFromWaspFileUri $ head uris of
      Nothing -> logM "[prismaSchemaFileChangeHandler] Could not get waspDir from file URI"
      Just waspDir -> do
        Prisma.analyzePrismaSchemaFileAndSetEntities waspDir
        handler $
          asks (^. State.waspFileUri) >>= \case
            Just uri -> do
              logM "[prismaSchemaFileChangeHandler] Updating missing diagnostics for schema.prisma"
              publishDiagnostics uri
            Nothing -> pure ()
