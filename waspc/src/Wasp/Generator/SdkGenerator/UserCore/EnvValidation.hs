module Wasp.Generator.SdkGenerator.UserCore.EnvValidation
  ( genEnvValidation,
    depsRequiredByEnvValidation,
  )
where

import Data.Aeson (KeyValue ((.=)), object)
import StrongPath (relfile)
import Wasp.AppSpec (AppSpec)
import qualified Wasp.AppSpec.App as AS.App
import qualified Wasp.AppSpec.App.Client as AS.App.Client
import qualified Wasp.AppSpec.App.Server as AS.App.Server
import Wasp.AppSpec.Valid (getApp)
import qualified Wasp.ExternalConfig.Npm.Dependency as Npm.Dependency
import Wasp.Generator.FileDraft (FileDraft)
import Wasp.Generator.Monad (Generator)
import Wasp.Generator.SdkGenerator.JsImport (extImportToImportJson)
import Wasp.Generator.SdkGenerator.UserCore.Common
  ( mkTmplFd,
    mkTmplFdWithData,
  )
import Wasp.Util ((<++>))

genEnvValidation :: AppSpec -> Generator [FileDraft]
genEnvValidation spec =
  genServerEnvFiles spec
    <++> genClientEnvFiles spec

genServerEnvFiles :: AppSpec -> Generator [FileDraft]
genServerEnvFiles spec = sequence [genServerEnv spec]

genClientEnvFiles :: AppSpec -> Generator [FileDraft]
genClientEnvFiles spec =
  sequence
    [ genClientEnvSchema spec,
      return . mkTmplFd $ [relfile|client/env.ts|]
    ]

genServerEnv :: AppSpec -> Generator FileDraft
genServerEnv spec = return $ mkTmplFdWithData [relfile|server/env.ts|] tmplData
  where
    tmplData =
      object
        [ "envValidationSchema" .= extImportToImportJson maybeEnvValidationSchema
        ]
    maybeEnvValidationSchema = AS.App.server app >>= AS.App.Server.envValidationSchema
    app = snd $ getApp spec

genClientEnvSchema :: AppSpec -> Generator FileDraft
genClientEnvSchema spec = return $ mkTmplFdWithData tmplPath tmplData
  where
    tmplPath = [relfile|client/env/schema.ts|]
    tmplData =
      object
        [ "envValidationSchema" .= extImportToImportJson maybeEnvValidationSchema
        ]
    maybeEnvValidationSchema = AS.App.client app >>= AS.App.Client.envValidationSchema
    app = snd $ getApp spec

depsRequiredByEnvValidation :: [Npm.Dependency.Dependency]
depsRequiredByEnvValidation =
  Npm.Dependency.fromList
    [ ("zod", "^3.23.8")
    ]
