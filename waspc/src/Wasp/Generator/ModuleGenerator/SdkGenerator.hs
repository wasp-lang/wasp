module Wasp.Generator.ModuleGenerator.SdkGenerator
  ( genModuleEntityExports,
    genModuleServerExports,
    genModuleServerTypes,
    genModuleOperationTypes,
    genModuleClientOperations,
    genModuleClientCrud,
    genModuleClientApi,
    genModuleClientAuth,
    genModuleSdkPackageJson,
  )
where

import Data.Aeson (object, (.=))
import qualified Data.Aeson as Aeson
import qualified Data.Char
import Data.List (intercalate)
import Data.Maybe (fromJust)
import StrongPath (File', Path', Rel, reldir, relfile, (</>))
import qualified StrongPath as SP
import Wasp.AppSpec.Module (EntityDeclaration (..), EntityRef (..), ModuleCrud (..), ModuleOperation (..), ModuleSpec (..))
import Wasp.Generator.Common (ProjectRootDir)
import Wasp.Generator.FileDraft (FileDraft, createTemplateFileDraft)
import Wasp.Generator.Templates (TemplatesDir)

mkModuleTmplFd ::
  Path' (Rel TemplatesDir) File' ->
  Path' (Rel ProjectRootDir) File' ->
  Maybe Aeson.Value ->
  FileDraft
mkModuleTmplFd tmplPath outPath = createTemplateFileDraft outPath tmplPath

-- Entity exports

genModuleEntityExports :: ModuleSpec -> [FileDraft]
genModuleEntityExports spec =
  [ mkModuleTmplFd
      ([relfile|module-sdk/entities/index.ts|])
      (SP.castFile [relfile|sdk/wasp/entities/index.ts|])
      ( Just $
          object
            [ "hasEntities" .= (not . null $ msEntities spec),
              "entities" .= map (\e -> object ["name" .= edName e]) (msEntities spec)
            ]
      )
  ]

-- Server exports (HttpError, config, MiddlewareConfigFn)

genModuleServerExports :: ModuleSpec -> [FileDraft]
genModuleServerExports _spec =
  [ mkModuleTmplFd
      [relfile|module-sdk/server/index.ts|]
      (SP.castFile [relfile|sdk/wasp/server/index.ts|])
      Nothing,
    -- Reuse HttpError and middleware types from the main SDK (no template vars)
    mkModuleTmplFd
      [relfile|sdk/wasp/server/HttpError.ts|]
      (SP.castFile [relfile|sdk/wasp/server/HttpError.ts|])
      Nothing,
    mkModuleTmplFd
      [relfile|sdk/wasp/server/middleware/globalMiddleware.ts|]
      (SP.castFile [relfile|sdk/wasp/server/middleware/globalMiddleware.ts|])
      Nothing
  ]

-- Server types — reuses the app SDK template (single source of truth for runtime types)

genModuleServerTypes :: ModuleSpec -> [FileDraft]
genModuleServerTypes _spec =
  [ mkModuleTmplFd
      [relfile|sdk/wasp/server/module/index.ts|]
      (SP.castFile [relfile|sdk/wasp/server/module/index.ts|])
      Nothing
  ]

-- Server operation types

genModuleOperationTypes :: ModuleSpec -> [FileDraft]
genModuleOperationTypes spec =
  concat
    [ genOpTypes "Query" (msQueries spec) [relfile|sdk/wasp/server/operations/queries/types.ts|],
      genOpTypes "Action" (msActions spec) [relfile|sdk/wasp/server/operations/actions/types.ts|]
    ]

genOpTypes :: String -> [(String, ModuleOperation)] -> Path' (Rel ProjectRootDir) File' -> [FileDraft]
genOpTypes _ [] _ = []
genOpTypes opKind ops outPath =
  [ mkModuleTmplFd
      ([relfile|module-sdk/server/operations/types.ts|])
      outPath
      ( Just $
          object
            [ "opKind" .= opKind,
              "operations" .= map (mkOpTypeData opKind) ops
            ]
      )
  ]

mkOpTypeData :: String -> (String, ModuleOperation) -> Aeson.Value
mkOpTypeData opKind (name, op) =
  object
    [ "typeName" .= capitalize name,
      "baseType"
        .= if moAuth op == Just True
          then "Authenticated" ++ opKind
          else opKind,
      "entityType" .= entityTypeStr (moEntities op)
    ]
  where
    entityTypeStr Nothing = "{}" :: String
    entityTypeStr (Just entities) =
      "{ " ++ intercalate "; " (map (\e -> erName e ++ ": any") entities) ++ " }"

-- Client operations

genModuleClientOperations :: ModuleSpec -> [FileDraft]
genModuleClientOperations spec =
  concat
    [ genClientQueryCore,
      genClientActionCore,
      genClientQueryIndex spec,
      genClientActionIndex spec,
      genClientOperationsIndex spec
    ]

genClientQueryCore :: [FileDraft]
genClientQueryCore =
  [ mkModuleTmplFd
      ([relfile|module-sdk/client/operations/queries/core.ts|])
      (SP.castFile [relfile|sdk/wasp/client/operations/queries/core.ts|])
      Nothing
  ]

genClientActionCore :: [FileDraft]
genClientActionCore =
  [ mkModuleTmplFd
      ([relfile|module-sdk/client/operations/actions/core.ts|])
      (SP.castFile [relfile|sdk/wasp/client/operations/actions/core.ts|])
      Nothing
  ]

genClientQueryIndex :: ModuleSpec -> [FileDraft]
genClientQueryIndex spec =
  [ mkModuleTmplFd
      ([relfile|module-sdk/client/operations/queries/index.ts|])
      (SP.castFile [relfile|sdk/wasp/client/operations/queries/index.ts|])
      ( Just $
          object
            [ "hasQueries" .= (not . null $ msQueries spec),
              "queries" .= map mkClientOpData (msQueries spec)
            ]
      )
  ]

genClientActionIndex :: ModuleSpec -> [FileDraft]
genClientActionIndex spec =
  [ mkModuleTmplFd
      ([relfile|module-sdk/client/operations/actions/index.ts|])
      (SP.castFile [relfile|sdk/wasp/client/operations/actions/index.ts|])
      ( Just $
          object
            [ "hasActions" .= (not . null $ msActions spec),
              "actions" .= map mkClientOpData (msActions spec)
            ]
      )
  ]

mkClientOpData :: (String, ModuleOperation) -> Aeson.Value
mkClientOpData (name, op) =
  object
    [ "name" .= name,
      "typeName" .= capitalize name,
      "entitiesArray" .= renderStringArray (maybe [] (map erName) (moEntities op))
    ]

genClientOperationsIndex :: ModuleSpec -> [FileDraft]
genClientOperationsIndex spec =
  [ mkModuleTmplFd
      ([relfile|module-sdk/client/operations/index.ts|])
      (SP.castFile [relfile|sdk/wasp/client/operations/index.ts|])
      ( Just $
          object
            [ "hasQueries" .= (not . null $ msQueries spec),
              "queryNames" .= intercalate ", " (map fst (msQueries spec)),
              "hasActions" .= (not . null $ msActions spec),
              "actionNames" .= intercalate ", " (map fst (msActions spec))
            ]
      )
  ]

-- Client CRUD

genModuleClientCrud :: ModuleSpec -> [FileDraft]
genModuleClientCrud spec =
  case msCruds spec of
    [] -> []
    cruds ->
      [ mkModuleTmplFd
          ([relfile|module-sdk/client/crud/operationsHelpers.ts|])
          (SP.castFile [relfile|sdk/wasp/client/crud/operationsHelpers.ts|])
          Nothing
      ]
        ++ map mkCrudFile cruds
        ++ [ mkModuleTmplFd
               ([relfile|module-sdk/client/crud/index.ts|])
               (SP.castFile [relfile|sdk/wasp/client/crud/index.ts|])
               ( Just $
                   object
                     [ "cruds" .= map (\(name, _) -> object ["name" .= name]) cruds
                     ]
               )
           ]
  where
    mkCrudFile (name, crud) =
      mkModuleTmplFd
        ([relfile|module-sdk/client/crud/_crud.ts|])
        (SP.castFile $ [reldir|sdk/wasp/client/crud|] </> fromJust (SP.parseRelFile (name ++ ".ts")))
        ( Just $
            object
              [ "name" .= name,
                "entityName" .= erName (mcEntity crud)
              ]
        )

genModuleClientApi :: ModuleSpec -> [FileDraft]
genModuleClientApi _spec =
  [ mkModuleTmplFd
      ([relfile|module-sdk/client/api.ts|])
      (SP.castFile [relfile|sdk/wasp/client/api.ts|])
      Nothing
  ]

genModuleClientAuth :: ModuleSpec -> [FileDraft]
genModuleClientAuth _spec =
  [ mkModuleTmplFd
      ([relfile|module-sdk/client/auth.ts|])
      (SP.castFile [relfile|sdk/wasp/client/auth.ts|])
      Nothing
  ]

-- SDK package.json (static, no template data)

genModuleSdkPackageJson :: ModuleSpec -> [FileDraft]
genModuleSdkPackageJson _spec =
  [ mkModuleTmplFd
      ([relfile|module-sdk/package.json|])
      (SP.castFile [relfile|sdk/wasp/package.json|])
      Nothing
  ]

-- Utilities

renderStringArray :: [String] -> String
renderStringArray items =
  "[" ++ intercalate ", " (map (\s -> "'" ++ s ++ "'") items) ++ "]"

capitalize :: String -> String
capitalize [] = []
capitalize (c : cs) = Data.Char.toUpper c : cs
