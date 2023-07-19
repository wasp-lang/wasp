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
import Wasp.Analyzer.Parser.SourceSpan (SourceSpan, spansOverlap)
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
  [ tsScaffoldActionProvider
  ]

-- | Provide 'LSP.CodeAction's that define missing JS/TS functions that do not
-- already exist but are needed by external imports that are found within the
-- given 'SourceSpan'.
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
  lookupExtImport extImport >>= \case
    ImportSyntaxError -> return []
    ImportCacheMiss -> return [] -- Not in cache, so we assume it's valid.
    ImportsSymbol _ _ -> return [] -- Valid import, no code action needed.
    ImportedFileDoesNotExist waspStylePath -> case einName extImport of
      Nothing -> return [] -- Syntax error in import, can't know if it's valid.
      Just symbolName -> createCodeActions True symbolName waspStylePath
    ImportedSymbolDoesNotExist symbolName waspStylePath -> createCodeActions False symbolName waspStylePath
  where
    createCodeActions :: Bool -> ExtImportName -> WaspStyleExtFilePath -> HandlerM [LSP.CodeAction]
    createCodeActions createNewFile symbolName waspStylePath = do
      let pathToExtImport = fromMaybe [] $ Inference.findExprPathToLocation src $ einLocation extImport
      possiblePaths <- getPossiblePaths createNewFile waspStylePath
      concat <$> mapM (createCodeAction symbolName pathToExtImport) possiblePaths

    -- Get the list of paths the client might want to define the function in. If
    -- a file with the right name already exists, it returns that one.
    --
    -- If no file exists, returns a list of files with each allowed extension,
    -- based on JS module resolution. For example, @import x from "@server/y.js"@
    -- results in both @y.ts@ and @y.js@.
    --
    -- See "Wasp.LSP.ExtImport.Path" for how allowed extensions are decided based
    -- on the path written in the Wasp source code.
    getPossiblePaths :: Bool -> WaspStyleExtFilePath -> HandlerM [SP.Path' SP.Abs SP.File']
    getPossiblePaths createNewFile waspStylePath = do
      let cachePathFromSrc =
            fromMaybe (error "[createCodeActions] unreachable: invalid wasp style path") $
              ExtImport.waspStylePathToCachePath waspStylePath
      -- The allowed extensions stored in the cache are based on what files exist
      -- on disk. Usually, paths in the cache will allow only one extension, which
      -- will be exactly the extension that is used on the file system.
      allowedExts <-
        asks (lookupKey cachePathFromSrc . (^. State.tsExports)) >>= \case
          Nothing -> pure $ ExtImport.allowedExts $ ExtImport.cachePathExtType cachePathFromSrc
          Just cachePathInCache -> pure $ ExtImport.allowedExts $ ExtImport.cachePathExtType cachePathInCache
      absPath <-
        fromMaybe (error "[createCodeActions] unreachable: can't get abs path")
          <$> ExtImport.cachePathToAbsPathWithoutExt cachePathFromSrc
      let possiblePaths =
            map (SP.castFile . fromMaybe (error "unreachable") . replaceExtension absPath) allowedExts
      filterPossiblePaths createNewFile possiblePaths

    -- Get a key as it is stored in a map. Useful when the 'Eq' instance is not
    -- structural equality.
    lookupKey :: Eq k => k -> M.HashMap k v -> Maybe k
    lookupKey k = find (== k) . M.keys

    -- Filters the list of paths so that the client doesn't get actions to create
    -- a new file when a file with the right name already exists.
    filterPossiblePaths :: Bool -> [SP.Path' SP.Abs SP.File'] -> HandlerM [SP.Path' SP.Abs SP.File']
    filterPossiblePaths True = return
    filterPossiblePaths False = filterM (liftIO . doesFileExist)

    -- Returns empty list if wasp.scaffold.ts-symbol does not have a template
    -- available for the request.
    createCodeAction :: ExtImportName -> Inference.ExprPath -> SP.Path' SP.Abs (SP.File a) -> HandlerM [LSP.CodeAction]
    createCodeAction symbolName pathToExtImport filepath = do
      -- The code action is nicer to use when we display just the relative path to the file.
      relFilepath <- maybe (SP.fromAbsFile filepath) SP.toFilePath <$> absFileInProjectRootDir filepath
      let args =
            ScaffoldTS.Args
              { ScaffoldTS.symbolName = symbolName,
                ScaffoldTS.pathToExtImport = pathToExtImport,
                ScaffoldTS.filepath = SP.castFile filepath
              }
          command = ScaffoldTS.lspCommand args
      let title = case symbolName of
            ExtImportModule _ -> printf "Add default export to %s" relFilepath
            ExtImportField name -> printf "Create function `%s` in %s" name relFilepath
      if ScaffoldTS.hasTemplateForArgs args
        then
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
        else return []
