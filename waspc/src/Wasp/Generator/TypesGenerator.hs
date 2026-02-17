module Wasp.Generator.TypesGenerator
  ( genTypes,
  )
where

import Data.Aeson (object, (.=))
import qualified Data.Aeson.Types as Aeson.Types
import StrongPath (reldir, relfile, (</>))
import Wasp.AppSpec (AppSpec, getActions, getCruds, getQueries)
import qualified Wasp.AppSpec.App as AS.App
import qualified Wasp.AppSpec.App.Auth as AS.Auth
import qualified Wasp.AppSpec.App.Client as AS.App.Client
import qualified Wasp.AppSpec.App.Db as AS.Db
import qualified Wasp.AppSpec.App.Server as AS.App.Server
import qualified Wasp.AppSpec.App.WebSocket as AS.App.WS
import qualified Wasp.AppSpec.Crud as AS.Crud
import qualified Wasp.AppSpec.Operation as AS.Operation
import Wasp.AppSpec.Valid (getApp, getIdFieldFromCrudEntity)
import Wasp.Generator.Crud (crudDeclarationToOperationsList, getCrudFilePath, getCrudOperationJson, makeCrudOperationKeyAndJsonPair)
import Wasp.Generator.FileDraft (FileDraft)
import Wasp.Generator.Monad (Generator)
import Wasp.Generator.TypesGenerator.Common (mkTmplFdWithData, mkTmplFdWithDstAndData)
import Wasp.Generator.TypesGenerator.JsImport (extImportToImportJson, extOperationImportToImportJson)
import qualified Wasp.Generator.WebSocket as AS.WS
import Wasp.Util ((<++>))

genTypes :: AppSpec -> Generator [FileDraft]
genTypes spec =
  genDbTypes spec
    <++> case maybeAuth of
      Nothing -> return []
      Just auth -> genAuthProviderTypes auth
    <++> genWebSocketTypes spec
    <++> genCrudTypes spec
    <++> genOperationTypes spec
    <++> genEnvTypes spec
  where
    maybeAuth = AS.App.auth $ snd $ getApp spec

genAuthProviderTypes :: AS.Auth.Auth -> Generator [FileDraft]
genAuthProviderTypes auth =
  return
    [ mkTmplFdWithData
        ([reldir|auth/providers|] </> [relfile|types.d.ts|])
        tmplData
    ]
  where
    tmplData =
      object
        [ "emailUserSignupFields" .= extImportToImportJson userEmailSignupFields,
          "usernameAndPasswordUserSignupFields" .= extImportToImportJson userUsernameAndPassowrdSignupFields
        ]
    userEmailSignupFields = AS.Auth.email authMethods >>= AS.Auth.userSignupFieldsForEmailAuth
    userUsernameAndPassowrdSignupFields = AS.Auth.usernameAndPassword authMethods >>= AS.Auth.userSignupFieldsForUsernameAuth
    authMethods = AS.Auth.methods auth

genDbTypes :: AppSpec -> Generator [FileDraft]
genDbTypes spec =
  case maybePrismaSetupFn of
    Nothing -> return []
    Just _ ->
      return
        [ mkTmplFdWithData
            ([reldir|db|] </> [relfile|types.d.ts|])
            tmplData
        ]
  where
    maybePrismaSetupFn = AS.App.db (snd $ getApp spec) >>= AS.Db.prismaSetupFn
    tmplData =
      object
        [ "prismaSetupFn" .= extImportToImportJson maybePrismaSetupFn
        ]

genWebSocketTypes :: AppSpec -> Generator [FileDraft]
genWebSocketTypes spec
  | AS.WS.areWebSocketsUsed spec =
      return
        [ mkTmplFdWithData
            ([reldir|webSocket|] </> [relfile|types.d.ts|])
            tmplData
        ]
  | otherwise = return []
  where
    maybeWebSocketFn = AS.App.WS.fn <$> (AS.App.webSocket $ snd $ getApp spec)
    tmplData =
      object
        [ "webSocketFn" .= extImportToImportJson maybeWebSocketFn
        ]

genCrudTypes :: AppSpec -> Generator [FileDraft]
genCrudTypes spec
  | null cruds = return []
  | otherwise = return $ map genCrudType cruds
  where
    cruds = getCruds spec

    genCrudType :: (String, AS.Crud.Crud) -> FileDraft
    genCrudType (name, crud) =
      mkTmplFdWithDstAndData
        [relfile|crud/_types.d.ts|]
        ([reldir|crud|] </> getCrudFilePath name "d.ts")
        (Just tmplData)
      where
        tmplData =
          object
            [ "crud" .= getCrudOperationJson name crud idField,
              "overrides" .= object overrides
            ]
        idField = getIdFieldFromCrudEntity spec crud

        overrides :: [Aeson.Types.Pair]
        overrides = map operationToOverrideImport crudOperations

        crudOperations = crudDeclarationToOperationsList crud

        operationToOverrideImport :: (AS.Crud.CrudOperation, AS.Crud.CrudOperationOptions) -> Aeson.Types.Pair
        operationToOverrideImport (operation, options) = makeCrudOperationKeyAndJsonPair operation importJson
          where
            importJson = extImportToImportJson $ AS.Crud.overrideFn options

genOperationTypes :: AppSpec -> Generator [FileDraft]
genOperationTypes spec
  | null queries && null actions = return []
  | otherwise =
      return $
        (if null queries then [] else [genQueryTypes])
          ++ (if null actions then [] else [genActionTypes])
  where
    queries = getQueries spec
    actions = getActions spec

    genQueryTypes :: FileDraft
    genQueryTypes =
      mkTmplFdWithDstAndData
        [relfile|operations/_types.d.ts|]
        ([reldir|operations|] </> [relfile|queries.d.ts|])
        (Just tmplData)
      where
        tmplData =
          object
            [ "operations" .= map mkOperationData (map (uncurry AS.Operation.QueryOp) queries)
            ]

    genActionTypes :: FileDraft
    genActionTypes =
      mkTmplFdWithDstAndData
        [relfile|operations/_types.d.ts|]
        ([reldir|operations|] </> [relfile|actions.d.ts|])
        (Just tmplData)
      where
        tmplData =
          object
            [ "operations" .= map mkOperationData (map (uncurry AS.Operation.ActionOp) actions)
            ]

    mkOperationData :: AS.Operation.Operation -> Aeson.Types.Value
    mkOperationData operation =
      object
        [ "jsFn" .= extOperationImportToImportJson (AS.Operation.getFn operation),
          "operationName" .= AS.Operation.getName operation
        ]

genEnvTypes :: AppSpec -> Generator [FileDraft]
genEnvTypes spec =
  return $
    (if hasServerEnvSchema then [genServerEnvType] else [])
      ++ (if hasClientEnvSchema then [genClientEnvType] else [])
  where
    app = snd $ getApp spec
    maybeServerEnvSchema = AS.App.server app >>= AS.App.Server.envValidationSchema
    maybeClientEnvSchema = AS.App.client app >>= AS.App.Client.envValidationSchema
    hasServerEnvSchema = maybe False (const True) maybeServerEnvSchema
    hasClientEnvSchema = maybe False (const True) maybeClientEnvSchema

    genServerEnvType :: FileDraft
    genServerEnvType =
      mkTmplFdWithDstAndData
        [relfile|env/server.d.ts|]
        ([reldir|env|] </> [relfile|server.d.ts|])
        (Just tmplData)
      where
        tmplData =
          object
            [ "envValidationSchema" .= extImportToImportJson maybeServerEnvSchema
            ]

    genClientEnvType :: FileDraft
    genClientEnvType =
      mkTmplFdWithDstAndData
        [relfile|env/client.d.ts|]
        ([reldir|env|] </> [relfile|client.d.ts|])
        (Just tmplData)
      where
        tmplData =
          object
            [ "envValidationSchema" .= extImportToImportJson maybeClientEnvSchema
            ]
