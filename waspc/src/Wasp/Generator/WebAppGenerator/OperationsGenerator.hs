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
import Wasp.Generator.FileDraft (FileDraft)
import qualified Wasp.Generator.ServerGenerator as ServerGenerator
import qualified Wasp.Generator.ServerGenerator.OperationsRoutesG as ServerOperationsRoutesG
import qualified Wasp.Generator.WebAppGenerator.Common as C
import qualified Wasp.Generator.WebAppGenerator.OperationsGenerator.ResourcesG as Resources
import Wasp.Wasp (Wasp)
import qualified Wasp.Wasp as Wasp
import qualified Wasp.Wasp.Action as Wasp.Action
import qualified Wasp.Wasp.Operation as Wasp.Operation
import qualified Wasp.Wasp.Query as Wasp.Query

genOperations :: Wasp -> [FileDraft]
genOperations wasp =
  concat
    [ genQueries wasp,
      genActions wasp,
      [C.makeSimpleTemplateFD (C.asTmplFile [relfile|src/operations/index.js|]) wasp],
      Resources.genResources wasp
    ]

genQueries :: Wasp -> [FileDraft]
genQueries wasp =
  map (genQuery wasp) (Wasp.getQueries wasp)
    ++ [C.makeSimpleTemplateFD (C.asTmplFile [relfile|src/queries/index.js|]) wasp]

genActions :: Wasp -> [FileDraft]
genActions wasp =
  map (genAction wasp) (Wasp.getActions wasp)

genQuery :: Wasp -> Wasp.Query.Query -> FileDraft
genQuery _ query = C.makeTemplateFD tmplFile dstFile (Just tmplData)
  where
    tmplFile = C.asTmplFile [relfile|src/queries/_query.js|]

    dstFile = C.asWebAppFile $ [reldir|src/queries/|] </> fromJust (getOperationDstFileName operation)
    tmplData =
      object
        [ "queryFnName" .= Wasp.Query._name query,
          "queryRoute"
            .= ( ServerGenerator.operationsRouteInRootRouter
                   ++ "/"
                   ++ ServerOperationsRoutesG.operationRouteInOperationsRouter operation
               ),
          "entitiesArray" .= makeJsArrayOfEntityNames operation
        ]
    operation = Wasp.Operation.QueryOp query

genAction :: Wasp -> Wasp.Action.Action -> FileDraft
genAction _ action = C.makeTemplateFD tmplFile dstFile (Just tmplData)
  where
    tmplFile = C.asTmplFile [relfile|src/actions/_action.js|]

    dstFile = C.asWebAppFile $ [reldir|src/actions/|] </> fromJust (getOperationDstFileName operation)
    tmplData =
      object
        [ "actionFnName" .= Wasp.Action._name action,
          "actionRoute"
            .= ( ServerGenerator.operationsRouteInRootRouter
                   ++ "/"
                   ++ ServerOperationsRoutesG.operationRouteInOperationsRouter operation
               ),
          "entitiesArray" .= makeJsArrayOfEntityNames operation
        ]
    operation = Wasp.Operation.ActionOp action

-- | Generates string that is JS array containing names (as strings) of entities being used by given operation.
--   E.g. "['Task', 'Project']"
makeJsArrayOfEntityNames :: Wasp.Operation.Operation -> String
makeJsArrayOfEntityNames operation = "[" ++ intercalate ", " entityStrings ++ "]"
  where
    entityStrings = maybe [] (map (\x -> "'" ++ x ++ "'")) (Wasp.Operation.getEntities operation)

getOperationDstFileName :: Wasp.Operation.Operation -> Maybe (Path' Rel' File')
getOperationDstFileName operation = parseRelFile (Wasp.Operation.getName operation ++ ".js")
