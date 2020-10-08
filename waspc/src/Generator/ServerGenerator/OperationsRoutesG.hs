module Generator.ServerGenerator.OperationsRoutesG
    ( genOperationsRoutes
    , operationRouteInOperationsRouter
    ) where

import           Data.Aeson                            (object, (.=))
import qualified Data.Aeson                            as Aeson
import           Data.Maybe                            (fromJust)
import qualified Path                                  as P
import qualified System.FilePath.Posix as FPPosix

import           Generator.FileDraft                   (FileDraft)
import qualified Generator.ServerGenerator.Common      as C
import           Generator.ServerGenerator.OperationsG (operationFileInSrcDir)
import           StrongPath                            (Dir, File, Path, Rel,
                                                        (</>))
import qualified StrongPath                            as SP
import qualified Util                                  as U
import           Wasp                                  (Wasp)
import qualified Wasp
import qualified Wasp.Action
import qualified Wasp.Operation
import qualified Wasp.Query
import qualified Wasp.Auth


genOperationsRoutes :: Wasp -> [FileDraft]
genOperationsRoutes wasp = concat
    [ map (genActionRoute wasp) (Wasp.getActions wasp)
    , map (genQueryRoute wasp) (Wasp.getQueries wasp)
    , [genOperationsRouter wasp]
    ]

genActionRoute :: Wasp -> Wasp.Action.Action -> FileDraft
genActionRoute wasp action = genOperationRoute wasp op tmplFile
    where op = Wasp.Operation.ActionOp action
          tmplFile = C.asTmplFile [P.relfile|src/routes/operations/_action.js|]

genQueryRoute :: Wasp -> Wasp.Query.Query -> FileDraft
genQueryRoute wasp query = genOperationRoute wasp op tmplFile
    where op = Wasp.Operation.QueryOp query
          tmplFile = C.asTmplFile [P.relfile|src/routes/operations/_query.js|]

genOperationRoute :: Wasp -> Wasp.Operation.Operation -> Path (Rel C.ServerTemplatesDir) File -> FileDraft
genOperationRoute wasp operation tmplFile = C.makeTemplateFD tmplFile dstFile (Just tmplData)
  where
    dstFile = operationsRoutesDirInServerRootDir </> operationRouteFileInOperationsRoutesDir operation

    baseTmplData = object
        [ "operationImportPath" .= operationImportPath
        , "operationName" .= Wasp.Operation.getName operation
        ]

    tmplData = case (Wasp.getAuth wasp) of
        Nothing -> baseTmplData
        Just auth -> U.jsonSet ("userEntityLower")
                               (Aeson.toJSON (U.toLowerFirst $ Wasp.Auth._userEntity auth))
                               baseTmplData

    operationImportPath = relPosixPathFromOperationsRoutesDirToSrcDir
        FPPosix.</> SP.toFilePath (SP.relFileToPosix' $ operationFileInSrcDir operation)

data OperationsRoutesDir

operationsRoutesDirInServerSrcDir :: Path (Rel C.ServerSrcDir) (Dir OperationsRoutesDir)
operationsRoutesDirInServerSrcDir = SP.fromPathRelDir [P.reldir|routes/operations/|]

operationsRoutesDirInServerRootDir :: Path (Rel C.ServerRootDir) (Dir OperationsRoutesDir)
operationsRoutesDirInServerRootDir = C.serverSrcDirInServerRootDir </> operationsRoutesDirInServerSrcDir

operationRouteFileInOperationsRoutesDir :: Wasp.Operation.Operation -> Path (Rel OperationsRoutesDir) File
operationRouteFileInOperationsRoutesDir operation = fromJust $ SP.parseRelFile $ Wasp.Operation.getName operation ++ ".js"

-- | TODO: Make this not hardcoded! Maybe even use StrongPath? But I can't because of ../../ .
relPosixPathFromOperationsRoutesDirToSrcDir :: FilePath -- Posix
relPosixPathFromOperationsRoutesDirToSrcDir = "../.."


genOperationsRouter :: Wasp -> FileDraft
genOperationsRouter wasp = C.makeTemplateFD tmplFile dstFile (Just tmplData)
  where
    tmplFile = C.asTmplFile [P.relfile|src/routes/operations/index.js|]
    dstFile = operationsRoutesDirInServerRootDir </> SP.fromPathRelFile [P.relfile|index.js|]
    operations = map Wasp.Operation.ActionOp (Wasp.getActions wasp)
                 ++ map Wasp.Operation.QueryOp (Wasp.getQueries wasp)
    tmplData = object
        [ "operationRoutes" .= map makeOperationRoute operations
        ]
    makeOperationRoute operation =
        let operationName = Wasp.Operation.getName operation
        in object
           [ "importIdentifier" .= operationName
           , "importPath" .= ("./" ++ SP.toFilePath (SP.relFileToPosix' $ operationRouteFileInOperationsRoutesDir operation))
           , "routePath" .= ("/" ++ operationRouteInOperationsRouter operation)
           ]

operationRouteInOperationsRouter :: Wasp.Operation.Operation -> String
operationRouteInOperationsRouter = U.camelToKebabCase . Wasp.Operation.getName
