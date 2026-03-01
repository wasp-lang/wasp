{-# LANGUAGE TypeApplications #-}

module Wasp.Generator.ServerGenerator.OperationsG
  ( genOperations,
    queryFileInSrcDir,
    actionFileInSrcDir,
    operationFileInSrcDir,
    getModuleEntityMaps,
    getEntityAliasForExtImport,
    resolveEntityRefWithAlias,
  )
where

import Data.Aeson (object, (.=))
import qualified Data.Aeson as Aeson
import Data.List (find, isPrefixOf)
import qualified Data.Map as Map
import Data.Maybe (fromJust, fromMaybe)
import StrongPath (Dir, File', Path, Path', Posix, Rel, reldir, reldirP, relfile, (</>))
import qualified StrongPath as SP
import Wasp.AppSpec (AppSpec)
import qualified Wasp.AppSpec as AS
import qualified Wasp.AppSpec.Action as AS.Action
import qualified Wasp.AppSpec.App as AS.App
import Wasp.AppSpec.ExtImport (ExtImport (path), ExtImportPath (..))
import qualified Wasp.AppSpec.Operation as AS.Operation
import qualified Wasp.AppSpec.Query as AS.Query
import qualified Wasp.AppSpec.Valid as AS.Valid
import Wasp.Generator.Common (makeJsonWithAliasEntityData, makeJsonWithEntityData)
import Wasp.Generator.FileDraft (FileDraft)
import Wasp.Generator.Monad (Generator)
import qualified Wasp.Generator.ServerGenerator.Common as C
import Wasp.Generator.ServerGenerator.JsImport (extImportToImportJson)
import Wasp.Util ((<++>))

genOperations :: AppSpec -> Generator [FileDraft]
genOperations spec =
  genQueries spec
    <++> genActions spec

genQueries :: AppSpec -> Generator [FileDraft]
genQueries spec = mapM (genQuery spec) $ AS.getQueries spec

genActions :: AppSpec -> Generator [FileDraft]
genActions spec = mapM (genAction spec) $ AS.getActions spec

genQuery :: AppSpec -> (String, AS.Query.Query) -> Generator FileDraft
genQuery spec (queryName, query) = return $ C.mkTmplFdWithDstAndData tmplFile dstFile (Just tmplData)
  where
    operation = AS.Operation.QueryOp queryName query
    tmplFile = C.asTmplFile [relfile|src/queries/_query.ts|]
    dstFile = C.serverSrcDirInServerRootDir </> queryFileInSrcDir queryName
    tmplData = operationTmplData (getModuleEntityMaps spec) operation

genAction :: AppSpec -> (String, AS.Action.Action) -> Generator FileDraft
genAction spec (actionName, action) = return $ C.mkTmplFdWithDstAndData tmplFile dstFile (Just tmplData)
  where
    operation = AS.Operation.ActionOp actionName action
    tmplFile = [relfile|src/actions/_action.ts|]
    dstFile = C.serverSrcDirInServerRootDir </> actionFileInSrcDir actionName
    tmplData = operationTmplData (getModuleEntityMaps spec) operation

queryFileInSrcDir :: String -> Path' (Rel C.ServerSrcDir) File'
queryFileInSrcDir queryName =
  [reldir|queries|]
    -- TODO: fromJust here could fail if there is some problem with the name, we should handle this.
    </> fromJust (SP.parseRelFile $ queryName ++ ".ts")

actionFileInSrcDir :: String -> Path' (Rel C.ServerSrcDir) File'
actionFileInSrcDir actionName =
  [reldir|actions|]
    -- TODO: fromJust here could fail if there is some problem with the name, we should handle this.
    </> fromJust (SP.parseRelFile $ actionName ++ ".ts")

operationFileInSrcDir :: AS.Operation.Operation -> Path' (Rel C.ServerSrcDir) File'
operationFileInSrcDir (AS.Operation.QueryOp name _) = queryFileInSrcDir name
operationFileInSrcDir (AS.Operation.ActionOp name _) = actionFileInSrcDir name

operationTmplData :: [AS.App.ModuleEntityMap] -> AS.Operation.Operation -> Aeson.Value
operationTmplData moduleEntityMaps operation =
  object
    [ "jsFn" .= extImportToImportJson relPathFromOperationsDirToServerSrcDir (Just $ AS.Operation.getFn operation),
      "entities"
        .= maybe
          []
          (map resolveEntityRef)
          (AS.Operation.getEntities operation)
    ]
  where
    relPathFromOperationsDirToServerSrcDir :: Path Posix (Rel importLocation) (Dir C.ServerSrcDir)
    relPathFromOperationsDirToServerSrcDir = [reldirP|../|]

    resolveEntityRef entityRef =
      resolveEntityRefWithAlias moduleEntityMaps (AS.Operation.getFn operation) entityRef

-- | Resolve an entity ref to JSON data, applying alias mapping if applicable.
-- Uses the ExtImport to determine the package name for alias lookup.
resolveEntityRefWithAlias :: [AS.App.ModuleEntityMap] -> ExtImport -> AS.Ref a -> Aeson.Value
resolveEntityRefWithAlias entityMaps fnImport entityRef =
  let realEntityName = AS.refName entityRef
   in case getEntityAliasForExtImport entityMaps fnImport realEntityName of
        Just alias -> makeJsonWithAliasEntityData alias realEntityName
        Nothing -> makeJsonWithEntityData realEntityName

-- | Look up the entity alias for a real entity name given an ExtImport.
-- For imports from a package (@pkg/...), looks up that package's entity map.
-- For app-local imports (@src/), searches all maps for a unique reverse mapping.
getEntityAliasForExtImport :: [AS.App.ModuleEntityMap] -> ExtImport -> String -> Maybe String
getEntityAliasForExtImport entityMaps fnImport realEntityName =
  case getPackageNameFromImport fnImport of
    Just pkgName -> do
      entityMap <- lookupEntityMap pkgName entityMaps
      alias <- reverseLookup realEntityName entityMap
      if alias /= realEntityName then Just alias else Nothing
    Nothing ->
      reverseLookupInAnyMap realEntityName entityMaps

getModuleEntityMaps :: AppSpec -> [AS.App.ModuleEntityMap]
getModuleEntityMaps spec =
  fromMaybe [] $ AS.App.moduleEntityMaps (snd $ AS.Valid.getApp spec)

getPackageNameFromImport :: ExtImport -> Maybe String
getPackageNameFromImport imp =
  case path imp of
    ExtImportPkgPath pkgPath -> Just $ extractPackageName pkgPath
    ExtImportSrcPath _ -> Nothing

-- | Extract the npm package name from a @pkg/ path.
-- Handles scoped packages (@scope/name/...) and unscoped (name/...).
extractPackageName :: String -> String
extractPackageName pkgPath
  | "@" `isPrefixOf` pkgPath =
      case break (== '/') (drop 1 pkgPath) of
        (scope, '/' : rest) -> '@' : scope ++ "/" ++ takeWhile (/= '/') rest
        _ -> pkgPath
  | otherwise =
      takeWhile (/= '/') pkgPath

lookupEntityMap :: String -> [AS.App.ModuleEntityMap] -> Maybe (Map.Map String String)
lookupEntityMap pkgName =
  fmap AS.App._memEntityMap . find ((== pkgName) . AS.App._memPackageName)

reverseLookup :: (Eq v) => v -> Map.Map k v -> Maybe k
reverseLookup val = fmap fst . Map.lookupMin . Map.filter (== val)

reverseLookupInAnyMap :: String -> [AS.App.ModuleEntityMap] -> Maybe String
reverseLookupInAnyMap realEntityName maps =
  case aliasesFromAllMaps of
    [alias] -> Just alias
    _ -> Nothing
  where
    aliasesFromAllMaps =
      [ alias
        | m <- maps,
          Just alias <- [reverseLookup realEntityName (AS.App._memEntityMap m)],
          alias /= realEntityName
      ]
