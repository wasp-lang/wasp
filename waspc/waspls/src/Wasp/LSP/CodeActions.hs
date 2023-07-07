{-# LANGUAGE RankNTypes #-}

module Wasp.LSP.CodeActions
  ( getCodeActionsInRange,
  )
where

import Control.Lens ((^.))
import Control.Monad (filterM, (<=<))
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Log.Class (MonadLog (logM))
import Control.Monad.Reader.Class (asks)
import Data.Foldable (find)
import qualified Data.HashMap.Strict as M
import Data.Maybe (fromMaybe)
import qualified Data.Text as Text
import qualified Language.LSP.Server as LSP
import qualified Language.LSP.Types as LSP
import qualified Path as P
import qualified StrongPath as SP
import qualified StrongPath.Path as SP
import Text.Printf (printf)
import Wasp.Analyzer.Parser.AST (ExtImportName (ExtImportField, ExtImportModule))
import Wasp.Analyzer.Parser.CST (SyntaxNode)
import qualified Wasp.Analyzer.Parser.CST as S
import Wasp.Analyzer.Parser.CST.Traverse (Traversal)
import qualified Wasp.Analyzer.Parser.CST.Traverse as T
import Wasp.Analyzer.Parser.SourceSpan (SourceSpan (SourceSpan), spansOverlap)
import qualified Wasp.LSP.Commands.ScaffoldTsSymbol as ScaffoldTS
import Wasp.LSP.ExtImport.ExportsCache (ExtImportLookupResult (..), lookupExtImport)
import Wasp.LSP.ExtImport.Path (WaspStyleExtFilePath)
import qualified Wasp.LSP.ExtImport.Path as ExtImport
import Wasp.LSP.ExtImport.Syntax (ExtImportNode (einLocation, einName), extImportAtLocation)
import Wasp.LSP.ServerMonads (HandlerM)
import qualified Wasp.LSP.ServerState as State
import Wasp.LSP.Syntax (lspRangeToSpan)
import qualified Wasp.LSP.TypeInference as Inference
import Wasp.Util.IO (doesFileExist)

getCodeActionsInRange :: LSP.Range -> HandlerM [LSP.CodeAction]
getCodeActionsInRange range = do
  logM $ "[getCodeActionsInRange] range=" <> show range
  src <- asks (^. State.currentWaspSource)
  maybeCst <- asks (^. State.cst)
  case maybeCst of
    Nothing -> pure []
    Just syntax -> do
      concat <$> mapM (\f -> f src syntax range) codeActionProviders

codeActionProviders :: [String -> [SyntaxNode] -> LSP.Range -> HandlerM [LSP.CodeAction]]
codeActionProviders =
  [ tsScaffoldActionProvider
  ]

-- | Provide 'LSP.CodeAction's for missing external imports.
tsScaffoldActionProvider :: String -> [SyntaxNode] -> LSP.Range -> HandlerM [LSP.CodeAction]
tsScaffoldActionProvider src syntax range = do
  -- VSCode (incorrectly) sends codeAction requests with ranges where the start
  -- position is equal to the end position. This range contains 0 characters,
  -- so we add a character to it.
  --
  -- NOTE: The LSP specification specifies that the end of a range is exclusive:
  -- https://microsoft.github.io/language-server-protocol/specifications/specification-3-16/#range
  let sourceSpan =
        let SourceSpan s e = lspRangeToSpan src range
         in if s == e then SourceSpan s (e + 1) else SourceSpan s e
  let extImports = map (extImportAtLocation src) $ collectExtImportNodesInSpan sourceSpan (T.fromSyntaxForest syntax)
  -- TODO(before merge): create real code actions, and only when there is a diagnostic
  -- that means i also need to connect each extimport to a diagnostic, hmm
  concat <$> mapM (findCodeActionsForExtImport src) extImports
  where
    -- Post-condition: all 'Traversal's returned have @kindAt t == ExtImport@.
    collectExtImportNodesInSpan :: SourceSpan -> Traversal -> [Traversal]
    collectExtImportNodesInSpan sourceSpan t =
      -- Only consider the given traversal if it is within the span.
      if spansOverlap (T.spanAt t) sourceSpan
        then case T.kindAt t of
          S.ExtImport -> [t]
          _ -> concatMap (collectExtImportNodesInSpan sourceSpan) $ T.children t
        else []

findCodeActionsForExtImport :: String -> ExtImportNode -> HandlerM [LSP.CodeAction]
findCodeActionsForExtImport src extImport = do
  {-
  1. Lookup up the extimport in the exports cache to see if it is valid.
  2. If it is, then no code actions need to be returned.
  3. Otherwise, path through the expression tree to the ext import.
  4. Use the extension type stored in the cache key to get a list of possible
     files the user might want to add the code to.
     i.   Abs path <.> each extension
     ii.  If at least one of those paths exists on disks, filter out all paths
         that do not exist.
     iii. Otherwise, use all the paths.
  5. For each file, return a code action runs the "wasp.scaffold.ts-symbol"
     command with the desired export name, the file path, and the expr path.
  -}
  lookupExtImport extImport >>= \case
    ImportSyntaxError -> return [] -- Syntax error in import.
    ImportCacheMiss -> return [] -- TODO: maybe start filling cache and send an in-progress notification to client?
    ImportsSymbol _ _ -> return [] -- Valid import.
    ImportedFileDoesNotExist waspStylePath -> case einName extImport of
      Nothing -> return [] -- Syntax error in import.
      Just symbolName -> createCodeActions True symbolName waspStylePath
    ImportedSymbolDoesNotExist symbolName waspStylePath -> createCodeActions False symbolName waspStylePath
  where
    createCodeActions :: Bool -> ExtImportName -> WaspStyleExtFilePath -> HandlerM [LSP.CodeAction]
    createCodeActions createNewFile symbolName waspStylePath = do
      -- 4. Get paths.
      let cachePathFromSrc =
            fromMaybe (error "[createCodeActions] unreachable: invalid wasp style path") $
              ExtImport.waspStylePathToCachePath waspStylePath
      maybeCachePathInCache <- asks (find (== cachePathFromSrc) . M.keys . (^. State.tsExports))
      let allowedExts = case maybeCachePathInCache of
            Nothing -> ExtImport.allowedExts $ ExtImport.cachePathExtType cachePathFromSrc
            Just cachePathInCache -> ExtImport.allowedExts $ ExtImport.cachePathExtType cachePathInCache
      logM $ "Allowed exts: " ++ show allowedExts
      absPath <- fromMaybe (error "[createCodeActions] unreachable: can't get abs path") <$> ExtImport.cachePathToAbsPathWithoutExt cachePathFromSrc
      let unfilteredPaths = map (ExtImport.replaceExtension absPath) allowedExts
      -- 4ii-iii. Filter the paths.
      logM $ "Checking paths: " ++ show unfilteredPaths
      filteredPaths <-
        if createNewFile
          then return unfilteredPaths
          else filterM (liftIO . doesFileExist) unfilteredPaths
      let pathToExtImport = fromMaybe [] $ Inference.findExprPathToLocation src $ einLocation extImport
      -- 5. Create a code action for each possible file.
      concat <$> mapM (createCodeAction symbolName pathToExtImport) filteredPaths

    -- Returns empty list if wasp.scaffold.ts-symbol does not have a template
    -- available for the request.
    createCodeAction :: ExtImportName -> Inference.ExprPath -> SP.Path' SP.Abs (SP.File a) -> HandlerM [LSP.CodeAction]
    createCodeAction symbolName pathToExtImport filepath = do
      -- Strip the root path from @filepath@, using the absolute path if the stripping can not be done.
      relFilepath <- maybe (SP.fromAbsFile filepath) P.toFilePath . (((`P.stripProperPrefix` SP.toPathAbsFile filepath) <=< P.parseAbsDir) =<<) <$> LSP.getRootPath
      let args =
            ScaffoldTS.Args
              { ScaffoldTS.symbolName = symbolName,
                ScaffoldTS.pathToExtImport = pathToExtImport,
                ScaffoldTS.filepath = SP.castFile filepath
              }
          command = ScaffoldTS.command args
      let title = case symbolName of
            ExtImportModule name -> printf "Add default export `%s` to %s" name relFilepath
            ExtImportField name -> printf "Create function `%s` in %s" name relFilepath
      ScaffoldTS.hasTemplateForArgs args >>= \case
        False -> return []
        True ->
          return
            [ LSP.CodeAction
                { _title = Text.pack title,
                  _kind = Just LSP.CodeActionQuickFix,
                  _diagnostics = Nothing, -- TODO(before merge): get diagnostics,
                  _isPreferred = Nothing,
                  _disabled = Nothing,
                  _edit = Nothing,
                  _command = Just command,
                  _xdata = Nothing
                }
            ]
