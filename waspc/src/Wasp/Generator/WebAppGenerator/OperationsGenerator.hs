{-# LANGUAGE TypeApplications #-}

module Wasp.Generator.WebAppGenerator.OperationsGenerator
  ( genOperations,
  )
where

import Data.Aeson
  ( object,
    (.=),
  )
import Data.List (intercalate)
import Data.Maybe (fromJust)
import StrongPath (File', Path', Rel', parseRelFile, reldir, relfile, (</>))
import Wasp.AppSpec (AppSpec)
import qualified Wasp.AppSpec as AS
import qualified Wasp.AppSpec.Action as AS.Action
import qualified Wasp.AppSpec.Core.Ref as AS.Ref
import qualified Wasp.AppSpec.Operation as AS.Operation
import qualified Wasp.AppSpec.Query as AS.Query
import Wasp.Generator.FileDraft (FileDraft)
import qualified Wasp.Generator.ServerGenerator as ServerGenerator
import qualified Wasp.Generator.ServerGenerator.OperationsRoutesG as ServerOperationsRoutesG
import qualified Wasp.Generator.WebAppGenerator.Common as C
import qualified Wasp.Generator.WebAppGenerator.OperationsGenerator.ResourcesG as Resources

genOperations :: AppSpec -> [FileDraft]
genOperations spec =
  concat
    [ genQueries spec,
      genActions spec,
      [C.copyTmplAsIs $ C.asTmplFile [relfile|src/operations/index.js|]],
      Resources.genResources (error "TODO: should be spec")
    ]

genQueries :: AppSpec -> [FileDraft]
genQueries spec =
  map (genQuery spec) (AS.getDecls @AS.Query.Query spec)
    ++ [C.copyTmplAsIs $ C.asTmplFile [relfile|src/queries/index.js|]]

genActions :: AppSpec -> [FileDraft]
genActions spec =
  map (genAction spec) (AS.getDecls @AS.Action.Action spec)

genQuery :: AppSpec -> (String, AS.Query.Query) -> FileDraft
genQuery _ (queryName, query) = C.makeTemplateFD tmplFile dstFile (Just tmplData)
  where
    tmplFile = C.asTmplFile [relfile|src/queries/_query.js|]

    dstFile = C.asWebAppFile $ [reldir|src/queries/|] </> fromJust (getOperationDstFileName operation)
    tmplData =
      object
        [ "queryFnName" .= queryName,
          "queryRoute"
            .= ( ServerGenerator.operationsRouteInRootRouter
                   ++ "/"
                   ++ ServerOperationsRoutesG.operationRouteInOperationsRouter (error "TODO: operation")
               ),
          "entitiesArray" .= makeJsArrayOfEntityNames operation
        ]
    operation = AS.Operation.QueryOp queryName query

genAction :: AppSpec -> (String, AS.Action.Action) -> FileDraft
genAction _ (actionName, action) = C.makeTemplateFD tmplFile dstFile (Just tmplData)
  where
    tmplFile = C.asTmplFile [relfile|src/actions/_action.js|]

    dstFile = C.asWebAppFile $ [reldir|src/actions/|] </> fromJust (getOperationDstFileName operation)
    tmplData =
      object
        [ "actionFnName" .= actionName,
          "actionRoute"
            .= ( ServerGenerator.operationsRouteInRootRouter
                   ++ "/"
                   ++ ServerOperationsRoutesG.operationRouteInOperationsRouter (error "TODO: operation")
               ),
          "entitiesArray" .= makeJsArrayOfEntityNames operation
        ]
    operation = AS.Operation.ActionOp actionName action

-- | Generates string that is JS array containing names (as strings) of entities being used by given operation.
--   E.g. "['Task', 'Project']"
makeJsArrayOfEntityNames :: AS.Operation.Operation -> String
makeJsArrayOfEntityNames operation = "[" ++ intercalate ", " entityStrings ++ "]"
  where
    entityStrings = maybe [] (map $ \x -> "'" ++ AS.Ref.refName x ++ "'") (AS.Operation.getEntities operation)

getOperationDstFileName :: AS.Operation.Operation -> Maybe (Path' Rel' File')
getOperationDstFileName operation = parseRelFile (AS.Operation.getName operation ++ ".js")
