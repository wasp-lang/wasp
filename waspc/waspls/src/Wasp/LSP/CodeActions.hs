module Wasp.LSP.CodeActions
  ( getCodeActionsInRange,
  )
where

import Control.Lens ((^.))
import Control.Monad (filterM)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader.Class (asks)
import Data.Foldable (find)
import qualified Data.HashMap.Strict as M
import Data.Maybe (fromMaybe)
import qualified Data.Text as Text
import qualified Language.LSP.Types as LSP
import qualified StrongPath as SP
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
import Wasp.LSP.Util (absFileInProjectRootDir)
import Wasp.Util.IO (doesFileExist)

-- | Runs all 'codeActionProviders' and concatenates their results.
getCodeActionsInRange :: LSP.Range -> HandlerM [LSP.CodeAction]
getCodeActionsInRange range = do
  src <- asks (^. State.currentWaspSource)
  maybeCst <- asks (^. State.cst)
  -- VSCode sends codeAction requests with ranges where the start
  -- position is equal to the end position. This range contains 0 characters,
  -- so we add a character to it.
  --
  -- The LSP specification specifies that the end of a range is exclusive:
  -- https://microsoft.github.io/language-server-protocol/specifications/specification-3-16/#range
  let sourceSpan =
        let SourceSpan s e = lspRangeToSpan src range
         in if s == e then SourceSpan s (e + 1) else SourceSpan s e
  case maybeCst of
    Nothing -> pure []
    Just syntax -> do
      concat <$> mapM (\f -> f src syntax sourceSpan) codeActionProviders

codeActionProviders :: [String -> [SyntaxNode] -> SourceSpan -> HandlerM [LSP.CodeAction]]
codeActionProviders =
  [ tsScaffoldActionProvider
  ]

-- | Provide 'LSP.CodeAction's for missing external imports. The code actions
-- run the "Wasp.LSP.Commands.ScaffoldTsSymbol" command to define the missing
-- function in a JS/TS file.
tsScaffoldActionProvider :: String -> [SyntaxNode] -> SourceSpan -> HandlerM [LSP.CodeAction]
tsScaffoldActionProvider src syntax sourceSpan = do
  let extImports = map (extImportAtLocation src) $ collectExtImportNodesInSpan (T.fromSyntaxForest syntax)
  concat <$> mapM (findCodeActionsForExtImport src) extImports
  where
    -- Post-condition: all 'Traversal's returned have @kindAt t == ExtImport@.
    collectExtImportNodesInSpan :: Traversal -> [Traversal]
    collectExtImportNodesInSpan t =
      -- Only consider the given traversal if it is within the span.
      if spansOverlap (T.spanAt t) sourceSpan
        then case T.kindAt t of
          S.ExtImport -> [t]
          _ -> concatMap collectExtImportNodesInSpan $ T.children t
        else []

-- | Finds code actions to create a JS/TS function for an external import. Returns
-- one code action for each JS/TS/JSX/TSX file a function can be created in.
--
-- This function also checks to make sure each code action, which runs the
-- @wasp.scaffold.ts-symbol@ command, has a scaffolding template that can be
-- used.
findCodeActionsForExtImport :: String -> ExtImportNode -> HandlerM [LSP.CodeAction]
findCodeActionsForExtImport src extImport = do
  -- 1. Lookup up the extimport in the exports cache to see if it is valid.
  -- 2. If it is, then no code actions need to be returned.
  -- 3. Otherwise, path through the expression tree to the ext import.
  -- 4. Use the extension type stored in the cache key to get a list of possible
  --    files the user might want to add the code to.
  --    i.   Create absolute paths with each extension.
  --    ii.  If at least one of those paths exists on disks, filter out all paths
  --         that do not exist.
  --    iii. Otherwise, use all the paths.
  -- 5. For each file, return a code action runs the @wasp.scaffold.ts-symbol@
  --    command with the desired export name, the file path, and the expr path.

  -- 1. Lookup.
  lookupExtImport extImport >>= \case
    ImportSyntaxError -> return [] -- Syntax error in import.
    ImportCacheMiss -> return [] -- 2. Not in cache, so we assume it's valid.
    ImportsSymbol _ _ -> return [] -- 2.
    ImportedFileDoesNotExist waspStylePath -> case einName extImport of
      Nothing -> return [] -- Syntax error in import.
      Just symbolName -> createCodeActions True symbolName waspStylePath -- 3.
    ImportedSymbolDoesNotExist symbolName waspStylePath -> createCodeActions False symbolName waspStylePath -- 3.
  where
    createCodeActions :: Bool -> ExtImportName -> WaspStyleExtFilePath -> HandlerM [LSP.CodeAction]
    createCodeActions createNewFile symbolName waspStylePath = do
      -- 4.
      let cachePathFromSrc =
            fromMaybe (error "[createCodeActions] unreachable: invalid wasp style path") $
              ExtImport.waspStylePathToCachePath waspStylePath
      maybeCachePathInCache <- asks (find (== cachePathFromSrc) . M.keys . (^. State.tsExports))
      let allowedExts = case maybeCachePathInCache of
            Nothing -> ExtImport.allowedExts $ ExtImport.cachePathExtType cachePathFromSrc
            Just cachePathInCache -> ExtImport.allowedExts $ ExtImport.cachePathExtType cachePathInCache
      absPath <- fromMaybe (error "[createCodeActions] unreachable: can't get abs path") <$> ExtImport.cachePathToAbsPathWithoutExt cachePathFromSrc
      let unfilteredPaths = map (ExtImport.replaceExtension absPath) allowedExts

      -- 4ii-iii.
      filteredPaths <-
        if createNewFile
          then return unfilteredPaths
          else filterM (liftIO . doesFileExist) unfilteredPaths
      let pathToExtImport = fromMaybe [] $ Inference.findExprPathToLocation src $ einLocation extImport

      -- 5.
      concat <$> mapM (createCodeAction symbolName pathToExtImport) filteredPaths

    -- Returns empty list if wasp.scaffold.ts-symbol does not have a template
    -- available for the request.
    createCodeAction :: ExtImportName -> Inference.ExprPath -> SP.Path' SP.Abs (SP.File a) -> HandlerM [LSP.CodeAction]
    createCodeAction symbolName pathToExtImport filepath = do
      -- Strip the root path from @filepath@, using the absolute path if the stripping can not be done.
      relFilepath <- maybe (SP.fromAbsFile filepath) SP.toFilePath <$> absFileInProjectRootDir filepath
      let args =
            ScaffoldTS.Args
              { ScaffoldTS.symbolName = symbolName,
                ScaffoldTS.pathToExtImport = pathToExtImport,
                ScaffoldTS.filepath = SP.castFile filepath
              }
          command = ScaffoldTS.command args
      let title = case symbolName of
            ExtImportModule _ -> printf "Add default export to %s" relFilepath
            ExtImportField name -> printf "Create function `%s` in %s" name relFilepath
      ScaffoldTS.hasTemplateForArgs args >>= \case
        False -> return []
        True ->
          return
            [ LSP.CodeAction
                { _title = Text.pack title,
                  _kind = Just LSP.CodeActionQuickFix,
                  _diagnostics = Nothing,
                  _isPreferred = Nothing,
                  _disabled = Nothing,
                  _edit = Nothing,
                  _command = Just command,
                  _xdata = Nothing
                }
            ]
