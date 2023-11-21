module Wasp.Generator.WebAppGenerator.JsImport where

import qualified Data.Aeson as Aeson
import StrongPath (Dir, Path, Posix, Rel)
import StrongPath.TH (reldirP)
import Wasp.AppSpec.ExtImport (ExtImport (..))
import qualified Wasp.AppSpec.ExtImport as EI
import qualified Wasp.Generator.JsImport as GJI
import Wasp.Generator.WebAppGenerator.Common (WebAppSrcDir)
import Wasp.JsImport
  ( JsImport,
    JsImportIdentifier,
    JsImportStatement,
  )
import qualified Wasp.JsImport as JI

extImportToImportJson ::
  Path Posix (Rel importLocation) (Dir WebAppSrcDir) ->
  Maybe ExtImport ->
  Aeson.Value
extImportToImportJson pathFromImportLocationToSrcDir maybeExtImport = GJI.jsImportToImportJson jsImport
  where
    jsImport = extImportToJsImport pathFromImportLocationToSrcDir <$> maybeExtImport

-- extImportToImportJson ::
--   Path Posix (Rel importLocation) (Dir WebAppSrcDir) ->
--   Maybe ExtImport ->
--   Aeson.Value
-- extImportToImportJson _ maybeExtImport = case maybeExtImport of
--   Nothing -> object ["isDefined" .= False]
--   Just extImport -> makeImportObject extImport
--   where
--     makeImportObject (ExtImport importName importPath) =
--       let importClause = makeImportClause importName
--           importPathStr = "ext-sdrc/" ++ SP.toFilePath importPath
--        in object
--             [ "isDefined" .= True,
--               "importStatement" .= ("import " ++ importClause ++ "from \"" ++ importPathStr ++ "\""),
--               "importIdentifier" .= importName
--             ]
--     makeImportClause = \case
--       EI.ExtImportModule name -> name
--       EI.ExtImportField name -> "{ " ++ name ++ "

getJsImportStmtAndIdentifier ::
  Path Posix (Rel importLocation) (Dir WebAppSrcDir) ->
  EI.ExtImport ->
  (JsImportStatement, JsImportIdentifier)
getJsImportStmtAndIdentifier pathFromImportLocationToSrcDir = JI.getJsImportStmtAndIdentifier . extImportToJsImport pathFromImportLocationToSrcDir

extImportToJsImport ::
  Path Posix (Rel importLocation) (Dir WebAppSrcDir) ->
  EI.ExtImport ->
  JsImport
extImportToJsImport = GJI.extImportToJsImport webAppExtDir
  where
    -- filip: read notes in ServerGenerator/JsImport.hs
    webAppExtDir = [reldirP|../../../../src|]
