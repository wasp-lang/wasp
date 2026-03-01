module Wasp.Generator.ModuleGenerator
  ( genModuleSdk,
  )
where

import Data.Aeson (object, (.=))
import qualified Data.Aeson.Text as Aeson.Text
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import StrongPath (File', Path', Rel, relfile)
import qualified StrongPath as SP
import Wasp.AppSpec.Module (ModuleSpec (..))
import Wasp.Generator.Common (ProjectRootDir)
import Wasp.Generator.FileDraft (FileDraft, createTemplateFileDraft)
import Wasp.Generator.ModuleGenerator.DbGenerator (genSyntheticPrismaSchema)
import Wasp.Generator.ModuleGenerator.SdkGenerator
  ( genModuleClientApi,
    genModuleClientAuth,
    genModuleClientCrud,
    genModuleClientOperations,
    genModuleEntityExports,
    genModuleOperationTypes,
    genModuleSdkPackageJson,
    genModuleServerExports,
    genModuleServerTypes,
  )
import Wasp.Generator.Templates (TemplatesDir)

genModuleSdk :: ModuleSpec -> [FileDraft]
genModuleSdk spec =
  concat
    [ genSyntheticPrismaSchema spec,
      genModuleEntityExports spec,
      genModuleServerExports spec,
      genModuleServerTypes spec,
      genModuleOperationTypes spec,
      genModuleClientOperations spec,
      genModuleClientCrud spec,
      genModuleClientApi spec,
      genModuleClientAuth spec,
      genModuleConfig spec,
      genModuleSdkPackageJson spec
    ]

genModuleConfig :: ModuleSpec -> [FileDraft]
genModuleConfig spec =
  [ createTemplateFileDraft
      (SP.castFile configOutPath)
      (SP.castFile configTmplPath)
      ( Just $
          object
            [ "packageNameJson" .= packageNameJson,
              "configJson" .= configJsonStr
            ]
      )
  ]
  where
    configOutPath :: Path' (Rel ProjectRootDir) File'
    configOutPath = SP.castFile [relfile|sdk/wasp/modules/config.ts|]

    configTmplPath :: Path' (Rel TemplatesDir) File'
    configTmplPath = SP.castFile [relfile|module-sdk/modules/config.ts|]

    packageNameJson = TL.toStrict $ Aeson.Text.encodeToLazyText (T.pack $ msPackageName spec)

    configJsonStr =
      let provides = msProvides spec
       in if Map.null provides
            then "{}" :: String
            else TL.unpack $ Aeson.Text.encodeToLazyText provides
