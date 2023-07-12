module Wasp.LSP.GotoDefinition
  ( gotoDefinitionOfSymbolAtPosition,
  )
where

import Control.Lens ((^.))
import Control.Monad.Log.Class (logM)
import Control.Monad.Reader.Class (asks)
import qualified Language.LSP.Types as LSP
import qualified Language.LSP.Types.Lens as LSP
import qualified StrongPath as SP
import Wasp.Analyzer.Parser.CST.Traverse (Traversal, fromSyntaxForest)
import qualified Wasp.Analyzer.Parser.CST.Traverse as T
import Wasp.Analyzer.Parser.SourceRegion (sourceSpanToRegion)
import qualified Wasp.LSP.ExtImport.ExportsCache as ExtImport
import qualified Wasp.LSP.ExtImport.Syntax as ExtImport
import Wasp.LSP.ServerMonads (HandlerM)
import qualified Wasp.LSP.ServerState as State
import Wasp.LSP.Syntax (locationAtOffset, lspPositionToOffset)
import Wasp.LSP.Util (waspSourceRegionToLspRange)
import qualified Wasp.TypeScript.Inspect.Exports as TS

definitionProviders :: [String -> Traversal -> HandlerM [LSP.LocationLink]]
definitionProviders = [extImportDefinitionProvider]

gotoDefinitionOfSymbolAtPosition :: LSP.Position -> HandlerM (LSP.List LSP.LocationLink)
gotoDefinitionOfSymbolAtPosition position = do
  src <- asks (^. State.currentWaspSource)
  maybeSyntax <- asks (^. State.cst)
  case maybeSyntax of
    Nothing -> return $ LSP.List [] -- No syntax tree, can't provide definitions.
    Just syntax -> do
      -- Run each definition provider and concatenate results.
      let offset = lspPositionToOffset src position
      let location = locationAtOffset offset (fromSyntaxForest syntax)
      definitionLocations <- concat <$> mapM (\f -> f src location) definitionProviders
      logM $ "Got definitions at " ++ show position ++ ": " ++ show definitionLocations
      return $ LSP.List definitionLocations

-- | If the provided location is within an ExtImport syntax node, returns the
-- location in the JS/TS file of the symbol that the ExtImport points to, if
-- that symbol is defined and the JS/TS file is in the cached export lists.
extImportDefinitionProvider :: String -> Traversal -> HandlerM [LSP.LocationLink]
extImportDefinitionProvider src location =
  case ExtImport.findExtImportAroundLocation src location of
    Nothing -> return [] -- Not at an external import.
    Just extImport -> do
      let extImportSpan = T.spanAt $ ExtImport.einLocation extImport
      let extImportRange = waspSourceRegionToLspRange $ sourceSpanToRegion src extImportSpan
      ExtImport.lookupExtImport extImport >>= \case
        ExtImport.ImportsSymbol tsFile tsExport -> do
          case TS.tsExportSourceRegion tsExport of
            Nothing -> return [link extImportRange $ gotoFile tsFile]
            Just sourceRegion -> return [link extImportRange $ gotoRangeInFile tsFile $ waspSourceRegionToLspRange sourceRegion]
        _ -> return [] -- Location does not point to a valid exported symbol.

-- | @link linkRange location@ creates a @LSP.LocationLink@ to the same place as
-- @location@ and sets the origin selection range (the range that is highlighted
-- in the editor in the original file) to @linkRange@.
link :: LSP.Range -> LSP.Location -> LSP.LocationLink
link linkRange gotoLocation =
  LSP.LocationLink
    { _originSelectionRange = Just linkRange,
      _targetUri = gotoLocation ^. LSP.uri,
      _targetRange = gotoLocation ^. LSP.range,
      _targetSelectionRange = gotoLocation ^. LSP.range
    }

-- | Create a 'LSP.Location' pointing to the start of a file.
--
-- This creates a location which points to an absurdly large range of text (1
-- million lines) so that the entire file is selected.
gotoFile :: SP.Path' SP.Abs (SP.File any) -> LSP.Location
gotoFile file = gotoRangeInFile file (LSP.Range (LSP.Position 0 0) (LSP.Position 1000000 0))

-- | Create a 'LSP.Location' pointing to a specific place in a file.
gotoRangeInFile :: SP.Path' SP.Abs (SP.File any) -> LSP.Range -> LSP.Location
gotoRangeInFile file range =
  let uri = LSP.filePathToUri $ SP.fromAbsFile file
   in LSP.Location {_uri = uri, _range = range}
