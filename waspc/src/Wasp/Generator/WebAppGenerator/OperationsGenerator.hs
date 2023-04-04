{-# LANGUAGE TypeApplications #-}

module Wasp.Generator.WebAppGenerator.OperationsGenerator
  ( genOperations,
  )
where

import Data.Aeson
  ( object,
    (.=),
  )
import Data.Aeson.Types (Pair)
import Data.List (intercalate)
import Data.Maybe (fromJust)
import StrongPath (File, File', Path, Path', Rel, Rel', parseRelFile, reldir, relfile, (</>))
import qualified StrongPath as SP
import StrongPath.Types (Posix)
import System.FilePath (splitExtension)
import Wasp.AppSpec (AppSpec)
import qualified Wasp.AppSpec as AS
import qualified Wasp.AppSpec.Action as AS.Action
import qualified Wasp.AppSpec.Operation as AS.Operation
import qualified Wasp.AppSpec.Query as AS.Query
import Wasp.Generator.FileDraft (FileDraft)
import Wasp.Generator.Monad (Generator)
import qualified Wasp.Generator.ServerGenerator as ServerGenerator
import Wasp.Generator.ServerGenerator.Common (serverSrcDirInServerRootDir)
import Wasp.Generator.ServerGenerator.OperationsG (operationFileInSrcDir)
import qualified Wasp.Generator.ServerGenerator.OperationsRoutesG as ServerOperationsRoutesG
import Wasp.Generator.WebAppGenerator.Common (serverRootDirFromWebAppRootDir)
import qualified Wasp.Generator.WebAppGenerator.Common as C
import qualified Wasp.Generator.WebAppGenerator.OperationsGenerator.ResourcesG as Resources
import Wasp.JsImport (JsImportName (JsImportField), getJsImportStmtAndIdentifier, makeJsImport)
import Wasp.Util (toUpperFirst, (<++>))

genOperations :: AppSpec -> Generator [FileDraft]
genOperations spec =
  genQueries spec
    <++> genActions spec
    <++> Resources.genResources spec
    <++> return
      [ C.mkSrcTmplFd [relfile|operations/index.ts|],
        C.mkSrcTmplFd [relfile|operations/updateHandlersMap.js|]
      ]

genQueries :: AppSpec -> Generator [FileDraft]
genQueries spec =
  mapM (genQuery spec) (AS.getQueries spec)
    <++> return
      [ C.mkSrcTmplFd [relfile|queries/index.js|],
        C.mkSrcTmplFd [relfile|queries/index.d.ts|],
        C.mkSrcTmplFd [relfile|queries/core.js|],
        C.mkSrcTmplFd [relfile|queries/core.d.ts|]
      ]

genActions :: AppSpec -> Generator [FileDraft]
genActions spec =
  mapM (genAction spec) (AS.getActions spec)
    <++> return
      [ C.mkSrcTmplFd [relfile|actions/index.ts|],
        C.mkSrcTmplFd [relfile|actions/core.js|],
        C.mkSrcTmplFd [relfile|actions/core.d.ts|]
      ]

genQuery :: AppSpec -> (String, AS.Query.Query) -> Generator FileDraft
genQuery _ (queryName, query) = return $ C.mkTmplFdWithDstAndData tmplFile dstFile (Just tmplData)
  where
    tmplFile = C.asTmplFile [relfile|src/queries/_query.ts|]

    dstFile = C.asWebAppFile $ [reldir|src/queries/|] </> fromJust (getOperationDstFileName operation)
    tmplData =
      object $
        [ "queryRoute"
            .= ( ServerGenerator.operationsRouteInRootRouter
                   ++ "/"
                   ++ ServerOperationsRoutesG.operationRouteInOperationsRouter operation
               ),
          "entitiesArray" .= makeJsArrayOfEntityNames operation
        ]
          ++ operationTypeData operation
    operation = AS.Operation.QueryOp queryName query

genAction :: AppSpec -> (String, AS.Action.Action) -> Generator FileDraft
genAction _ (actionName, action) = return $ C.mkTmplFdWithDstAndData tmplFile dstFile (Just tmplData)
  where
    tmplFile = C.asTmplFile [relfile|src/actions/_action.ts|]

    dstFile = C.asWebAppFile $ [reldir|src/actions/|] </> fromJust (getOperationDstFileName operation)
    tmplData =
      object $
        [ "actionRoute"
            .= ( ServerGenerator.operationsRouteInRootRouter
                   ++ "/"
                   ++ ServerOperationsRoutesG.operationRouteInOperationsRouter operation
               ),
          "entitiesArray" .= makeJsArrayOfEntityNames operation
        ]
          ++ operationTypeData operation
    operation = AS.Operation.ActionOp actionName action

operationTypeData :: AS.Operation.Operation -> [Pair]
operationTypeData operation = tmplData
  where
    tmplData =
      [ "operationTypeImportStmt" .= (operationTypeImportStmt :: String),
        "operationTypeName" .= (operationTypeImportIdentifier :: String)
      ]

    (operationTypeImportStmt, operationTypeImportIdentifier) =
      getJsImportStmtAndIdentifier $
        makeJsImport operationImportPath (JsImportField $ toUpperFirst operationName)

    operationName = AS.Operation.getName operation

    operationImportPath =
      toViteImportPath $
        fromJust $
          SP.relFileToPosix serverOperationFileFromWebAppOperationsDir

    serverOperationFileFromWebAppOperationsDir =
      webAppRootDirFromWebAppOperationsDir </> serverOperationFileFromWebAppRootDir
    webAppRootDirFromWebAppOperationsDir = [reldir|../..|]
    serverOperationFileFromWebAppRootDir = serverRootDirFromWebAppRootDir </> serverOperationFileInServerRootDir
    serverOperationFileInServerRootDir = serverSrcDirInServerRootDir </> operationFileInSrcDir operation

toViteImportPath :: Path Posix (Rel r) (File f) -> Path Posix (Rel r) (File f)
toViteImportPath = fromJust . SP.parseRelFileP . dropExtension . SP.fromRelFileP
  where
    dropExtension = fst . splitExtension

-- | Generates string that is JS array containing names (as strings) of entities being used by given operation.
--   E.g. "['Task', 'Project']"
makeJsArrayOfEntityNames :: AS.Operation.Operation -> String
makeJsArrayOfEntityNames operation = "[" ++ intercalate ", " entityStrings ++ "]"
  where
    entityStrings = maybe [] (map $ \x -> "'" ++ AS.refName x ++ "'") (AS.Operation.getEntities operation)

getOperationDstFileName :: AS.Operation.Operation -> Maybe (Path' Rel' File')
getOperationDstFileName operation = parseRelFile (AS.Operation.getName operation ++ ".ts")
