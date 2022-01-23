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
import qualified Wasp.AppSpec.Operation as AS.Operation
import qualified Wasp.AppSpec.Query as AS.Query
import Wasp.Generator.FileDraft (FileDraft)
import Wasp.Generator.Monad (Generator)
import qualified Wasp.Generator.ServerGenerator as ServerGenerator
import qualified Wasp.Generator.ServerGenerator.OperationsRoutesG as ServerOperationsRoutesG
import qualified Wasp.Generator.WebAppGenerator.Common as C
import qualified Wasp.Generator.WebAppGenerator.OperationsGenerator.ResourcesG as Resources
import Wasp.Util ((<++>))

genOperations :: AppSpec -> Generator [FileDraft]
genOperations spec =
  genQueries spec
    <++> genActions spec
    <++> return [C.mkTmplFd $ C.asTmplFile [relfile|src/operations/index.js|]]
    <++> Resources.genResources spec

genQueries :: AppSpec -> Generator [FileDraft]
genQueries spec = do
  queriesFds <- mapM (genQuery spec) (AS.getQueries spec)
  return $ queriesFds ++ [C.mkTmplFd $ C.asTmplFile [relfile|src/queries/index.js|]]

genActions :: AppSpec -> Generator [FileDraft]
genActions spec =
  mapM (genAction spec) (AS.getActions spec)

genQuery :: AppSpec -> (String, AS.Query.Query) -> Generator FileDraft
genQuery _ (queryName, query) = return $ C.mkTmplFdWithDstAndData tmplFile dstFile (Just tmplData)
  where
    tmplFile = C.asTmplFile [relfile|src/queries/_query.js|]

    dstFile = C.asWebAppFile $ [reldir|src/queries/|] </> fromJust (getOperationDstFileName operation)
    tmplData =
      object
        [ "queryFnName" .= (queryName :: String),
          "queryRoute"
            .= ( ServerGenerator.operationsRouteInRootRouter
                   ++ "/"
                   ++ ServerOperationsRoutesG.operationRouteInOperationsRouter operation
               ),
          "entitiesArray" .= makeJsArrayOfEntityNames operation
        ]
    operation = AS.Operation.QueryOp queryName query

genAction :: AppSpec -> (String, AS.Action.Action) -> Generator FileDraft
genAction _ (actionName, action) = return $ C.mkTmplFdWithDstAndData tmplFile dstFile (Just tmplData)
  where
    tmplFile = C.asTmplFile [relfile|src/actions/_action.js|]

    dstFile = C.asWebAppFile $ [reldir|src/actions/|] </> fromJust (getOperationDstFileName operation)
    tmplData =
      object
        [ "actionFnName" .= (actionName :: String),
          "actionRoute"
            .= ( ServerGenerator.operationsRouteInRootRouter
                   ++ "/"
                   ++ ServerOperationsRoutesG.operationRouteInOperationsRouter operation
               ),
          "entitiesArray" .= makeJsArrayOfEntityNames operation
        ]
    operation = AS.Operation.ActionOp actionName action

-- | Generates string that is JS array containing names (as strings) of entities being used by given operation.
--   E.g. "['Task', 'Project']"
makeJsArrayOfEntityNames :: AS.Operation.Operation -> String
makeJsArrayOfEntityNames operation = "[" ++ intercalate ", " entityStrings ++ "]"
  where
    entityStrings = maybe [] (map $ \x -> "'" ++ AS.refName x ++ "'") (AS.Operation.getEntities operation)

getOperationDstFileName :: AS.Operation.Operation -> Maybe (Path' Rel' File')
getOperationDstFileName operation = parseRelFile (AS.Operation.getName operation ++ ".js")
