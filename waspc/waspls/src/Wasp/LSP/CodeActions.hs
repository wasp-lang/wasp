module Wasp.LSP.CodeActions
  ( getCodeActionsInRange,
  )
where

import Control.Lens ((^.))
import Control.Monad (filterM)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader.Class (asks)
import Data.Maybe (catMaybes, fromMaybe)
import qualified Data.Text as Text
import qualified Language.LSP.Types as LSP
import qualified StrongPath as SP
import Text.Printf (printf)
import Wasp.Analyzer.Parser.AST (ExtImportName (ExtImportField, ExtImportModule))
import Wasp.Analyzer.Parser.CST (SyntaxNode)
import qualified Wasp.Analyzer.Parser.CST as S
import Wasp.Analyzer.Parser.CST.Traverse (Traversal)
import qualified Wasp.Analyzer.Parser.CST.Traverse as T
import Wasp.Analyzer.Parser.SourceSpan (SourceSpan, spansOverlap)
import qualified Wasp.LSP.Commands.ScaffoldTsSymbol as ScaffoldTS
import Wasp.LSP.ExtImport.ExportsCache (ExtImportLookupResult (..), lookupExtImport)
import Wasp.LSP.ExtImport.Path (WaspStyleExtFilePath)
import qualified Wasp.LSP.ExtImport.Path as ExtImport
import Wasp.LSP.ExtImport.Syntax (ExtImportNode (einLocation, einName), extImportAtLocation)
import Wasp.LSP.ServerMonads (HandlerM, logM)
import qualified Wasp.LSP.ServerState as State
import Wasp.LSP.Syntax (lspRangeToSpan)
import qualified Wasp.LSP.TypeInference as Inference
import Wasp.LSP.Util (getPathRelativeToProjectDir)
import qualified Wasp.Util.HashMap as M
import Wasp.Util.IO (doesFileExist)
import Wasp.Util.StrongPath (replaceExtension)

-- | Runs all 'codeActionProviders' and concatenates their results.
getCodeActionsInRange :: LSP.Range -> HandlerM [LSP.CodeAction]
getCodeActionsInRange range = do
  src <- asks (^. State.currentWaspSource)
  maybeCst <- asks (^. State.cst)
  let sourceSpan = lspRangeToSpan src range
  case maybeCst of
    Nothing -> pure []
    Just syntax -> do
      concat <$> mapM (\provider -> provider src syntax sourceSpan) codeActionProviders

codeActionProviders :: [String -> [SyntaxNode] -> SourceSpan -> HandlerM [LSP.CodeAction]]
codeActionProviders =
  [ extImportScaffoldActionProvider
  ]

-- | Provide 'LSP.CodeAction's that define missing JS/TS functions that do not
-- already exist but are needed by external imports that are found within the
-- given 'SourceSpan'.
extImportScaffoldActionProvider :: String -> [SyntaxNode] -> SourceSpan -> HandlerM [LSP.CodeAction]
extImportScaffoldActionProvider src syntax sourceSpan = do
  let extImports = map (extImportAtLocation src) $ collectExtImportNodesInSpan (T.fromSyntaxForest syntax)
  concat <$> mapM (getScaffoldActionsForExtImport src) extImports
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
getScaffoldActionsForExtImport :: String -> ExtImportNode -> HandlerM [LSP.CodeAction]
getScaffoldActionsForExtImport src extImport = do
  lookupExtImport extImport >>= \case
    ImportSyntaxError -> return []
    ImportCacheMiss -> return [] -- Not in cache, so we assume it's valid.
    ImportsSymbol _ _ -> return [] -- Valid import, no code action needed.
    ImportedFileDoesNotExist waspStylePath -> case einName extImport of
      Nothing -> return [] -- Syntax error in import, can't know if it's valid.
      Just symbolName -> makeCodeActions True symbolName waspStylePath
    ImportedSymbolDoesNotExist symbolName waspStylePath -> makeCodeActions False symbolName waspStylePath
  where
    makeCodeActions :: Bool -> ExtImportName -> WaspStyleExtFilePath -> HandlerM [LSP.CodeAction]
    makeCodeActions createNewFile symbolName waspStylePath = do
      let pathToExtImport = fromMaybe [] $ Inference.findExprPathToLocation src $ einLocation extImport
      referencedPaths <- getPathsReferencedByExtImport createNewFile waspStylePath
      catMaybes <$> mapM (makeCodeAction symbolName pathToExtImport) referencedPaths

    -- Get the list of paths the client might want to define the function in. If
    -- a file with the right name already exists, it returns that one.
    --
    -- If no file exists, returns a list of files with each allowed extension,
    -- based on JS module resolution. For example, @import x from "@server/y.js"@
    -- results in both @y.ts@ and @y.js@.
    --
    -- See "Wasp.LSP.ExtImport.Path" for how allowed extensions are decided based
    -- on the path written in the Wasp source code.
    getPathsReferencedByExtImport :: Bool -> WaspStyleExtFilePath -> HandlerM [SP.Path' SP.Abs SP.File']
    getPathsReferencedByExtImport createNewFile waspStylePath = do
      let cachePathFromSrc =
            fromMaybe (error "[createCodeActions] unreachable: invalid wasp style path") $
              ExtImport.waspStylePathToCachePath waspStylePath
      -- The allowed extensions stored in the cache are based on what files exist
      -- on disk. Usually, paths in the cache will allow only one extension, which
      -- will be exactly the extension that is used on the file system.
      allowedExts <-
        ExtImport.allowedExts . ExtImport.cachePathExtType . fromMaybe cachePathFromSrc
          <$> asks (M.lookupKey cachePathFromSrc . (^. State.tsExports))
      absPath <-
        fromMaybe (error "[createCodeActions] unreachable: can't get abs path")
          <$> ExtImport.cachePathToAbsPathWithoutExt cachePathFromSrc
      let possiblePaths =
            map (SP.castFile . fromMaybe (error "unreachable") . replaceExtension absPath) allowedExts

      -- If not creating a new file, the external import could only be referencing an
      -- existing file, so nonexistant files are filtered.
      if createNewFile
        then return possiblePaths
        else filterM (liftIO . doesFileExist) possiblePaths

    -- Checks if the "ScaffoldTS" command has a template for this request; if it
    -- does not, returns 'Nothing'.
    makeCodeAction :: ExtImportName -> Inference.ExprPath -> SP.Path' SP.Abs (SP.File a) -> HandlerM (Maybe LSP.CodeAction)
    makeCodeAction symbolName pathToExtImport targetFile = do
      -- The code action is nicer to use when we display just the relative path to the file.
      targetFileForDisplay <- maybe (SP.fromAbsFile targetFile) SP.fromRelFile <$> getPathRelativeToProjectDir targetFile
      let args =
            ScaffoldTS.Args
              { ScaffoldTS.symbolName = symbolName,
                ScaffoldTS.pathToExtImport = pathToExtImport,
                ScaffoldTS.filepath = SP.castFile targetFile
              }
          command = ScaffoldTS.makeLspCommand args
      let title = case symbolName of
            ExtImportModule _ -> printf "Add default export to %s" targetFileForDisplay
            ExtImportField name -> printf "Create function `%s` in %s" name targetFileForDisplay
      if ScaffoldTS.hasTemplateForArgs args
        then
          return . Just $
            LSP.CodeAction
              { _title = Text.pack title,
                _kind = Just LSP.CodeActionQuickFix,
                _diagnostics = Nothing,
                _isPreferred = Nothing,
                _disabled = Nothing,
                _edit = Nothing,
                _command = Just command,
                _xdata = Nothing
              }
        else do
          logM $ "[makeCodeAction] Ignoring scaffold action with no available template: " ++ show args
          return Nothing
