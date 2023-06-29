{-# LANGUAGE DataKinds #-}

module Wasp.LSP.DynamicHandlers
  ( registerDynamicCapabilities,
  )
where

import Control.Lens ((.~), (^.))
import Control.Monad (forM_, (<=<))
import Control.Monad.Log.Class (logM)
import Control.Monad.Reader.Class (asks)
import Data.List (stripPrefix)
import Data.Maybe (mapMaybe)
import qualified Data.Text as T
import qualified Language.LSP.Server as LSP
import qualified Language.LSP.Types as LSP
import qualified Language.LSP.Types.Lens as LSP
import qualified StrongPath as SP
import Wasp.LSP.Analysis (publishDiagnostics)
import Wasp.LSP.ExtImport.Diagnostic (updateMissingExtImportDiagnostics)
import Wasp.LSP.ExtImport.ExportsCache (refreshExportsOfFiles)
import Wasp.LSP.ServerM (ServerM, handler, modify, sendToReactor)
import qualified Wasp.LSP.ServerState as State

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
    [ registerSourceFileWatcher
    ]

-- | Register file watcher watcher for JS and TS files in the src/ directory.
-- When these files change, the export lists for the changed files are
-- automatically refreshed.
--
-- Note that this registration is guaranteed: if it fails, places in @waspls@ that
-- rely on up-to-date export lists need to manually refresh the export lists.
registerSourceFileWatcher :: ServerM ()
registerSourceFileWatcher = do
  watchSourceFilesToken <-
    LSP.registerCapability
      LSP.SWorkspaceDidChangeWatchedFiles
      LSP.DidChangeWatchedFilesRegistrationOptions
        { _watchers =
            LSP.List
              [ LSP.FileSystemWatcher
                  { -- TODO(before merge): try again to see if i can get this working limited to src/ directory.
                    -- Earlier testing showed that "src/**/*.{ts,tsx,js,jsx}" didn't work 🤷‍♂️
                    _globPattern = "**/*.{ts,tsx,js,jsx}",
                    _kind = Nothing
                  }
              ]
        }
      sourceFilesChangedHandler
  case watchSourceFilesToken of
    Nothing -> logM "[initializedHandler] Client did not accept WorkspaceDidChangeWatchedFiles registration"
    Just _ -> logM "[initializedHandler] WorkspaceDidChangeWatchedFiles registered for JS/TS source files"
  modify (State.regTokens . State.watchSourceFilesToken .~ watchSourceFilesToken)

-- | Ran when files in src/ change. It refreshes the relevant export lists in
-- the cache and updates missing import diagnostics.
--
-- Both of these tasks are ran in the reactor thread so that other requests
-- can still be answered.
sourceFilesChangedHandler :: LSP.Handler ServerM 'LSP.WorkspaceDidChangeWatchedFiles
sourceFilesChangedHandler msg = do
  let (LSP.List uris) = fmap (^. LSP.uri) $ msg ^. LSP.params . LSP.changes
  logM $ "[watchSourceFilesHandler] Received file changes: " ++ show uris
  let fileUris = mapMaybe (SP.parseAbsFile <=< stripPrefix "file://" . T.unpack . LSP.getUri) uris
  forM_ fileUris $ \file -> sendToReactor $ do
    -- Refresh export list for modified file
    refreshExportsOfFiles [file]
    -- Update diagnostics for the wasp file
    updateMissingExtImportDiagnostics
    handler $
      asks (^. State.waspFileUri) >>= \case
        Just uri -> do
          logM $ "[watchSourceFilesHandler] Updating missing diagnostics for " ++ show uri
          publishDiagnostics uri
        Nothing -> pure ()
