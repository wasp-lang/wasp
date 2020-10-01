module Generator.WebAppGenerator.OperationsGenerator
    ( genOperations
    ) where

import           Data.Aeson                                  (object, (.=))
import           Data.Maybe                                  (fromJust)
import qualified Path                                        as P

import           Generator.FileDraft                         (FileDraft)
import qualified Generator.ServerGenerator                   as ServerGenerator
import qualified Generator.ServerGenerator.OperationsRoutesG as ServerOperationsRoutesG
import qualified Generator.WebAppGenerator.Common            as C
import           Wasp                                        (Wasp)
import qualified Wasp
import qualified Wasp.Action
import qualified Wasp.Operation
import qualified Wasp.Query


genOperations :: Wasp -> [FileDraft]
genOperations wasp = concat
    [ genQueries wasp
    , genActions wasp
    , [C.makeSimpleTemplateFD (C.asTmplFile [P.relfile|src/operations/index.js|]) wasp]
    ]

genQueries :: Wasp -> [FileDraft]
genQueries wasp = concat
    [ map (genQuery wasp) (Wasp.getQueries wasp)
    , [C.makeSimpleTemplateFD (C.asTmplFile [P.relfile|src/queries/index.js|]) wasp]
    ]

genActions :: Wasp -> [FileDraft]
genActions wasp = concat
    [ map (genAction wasp) (Wasp.getActions wasp)
    ]

genQuery :: Wasp -> Wasp.Query.Query -> FileDraft
genQuery _ query = C.makeTemplateFD tmplFile dstFile (Just tmplData)
  where
    tmplFile = C.asTmplFile [P.relfile|src/queries/_query.js|]
    -- | TODO: fromJust here could fail if there is some problem with the name, we should handle this.
    dstFile = C.asWebAppFile $ [P.reldir|src/queries/|] P.</> fromJust (getOperationDstFileName operation)
    tmplData = object
        [ "queryFnName" .= Wasp.Query._name query
        , "queryRoute" .=
            (ServerGenerator.operationsRouteInRootRouter
             ++ "/" ++ ServerOperationsRoutesG.operationRouteInOperationsRouter operation)
        ]
    operation = Wasp.Operation.QueryOp query

genAction :: Wasp -> Wasp.Action.Action -> FileDraft
genAction _ action = C.makeTemplateFD tmplFile dstFile (Just tmplData)
  where
    tmplFile = C.asTmplFile [P.relfile|src/actions/_action.js|]
    -- | TODO: fromJust here could fail if there is some problem with the name, we should handle this.
    dstFile = C.asWebAppFile $ [P.reldir|src/actions/|] P.</> fromJust (getOperationDstFileName operation)
    tmplData = object
        [ "actionFnName" .= Wasp.Action._name action
        , "actionRoute" .=
            (ServerGenerator.operationsRouteInRootRouter
             ++ "/" ++ ServerOperationsRoutesG.operationRouteInOperationsRouter operation)
        ]
    operation = Wasp.Operation.ActionOp action

getOperationDstFileName :: Wasp.Operation.Operation -> Maybe (P.Path P.Rel P.File)
getOperationDstFileName operation = P.parseRelFile (Wasp.Operation.getName operation ++ ".js")
