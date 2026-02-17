module Wasp.Generator.TypesGenerator
  ( genTypes,
  )
where

import Data.Aeson (object, (.=))
import StrongPath (reldir, relfile, (</>))
import Wasp.AppSpec (AppSpec)
import qualified Wasp.AppSpec.App as AS.App
import qualified Wasp.AppSpec.App.Auth as AS.Auth
import Wasp.AppSpec.Valid (getApp)
import Wasp.Generator.FileDraft (FileDraft)
import Wasp.Generator.Monad (Generator)
import Wasp.Generator.SdkGenerator.JsImport (extImportToImportJson)
import Wasp.Generator.TypesGenerator.Common (mkTmplFdWithData)

genTypes :: AppSpec -> Generator [FileDraft]
genTypes spec =
  case maybeAuth of
    Nothing -> return []
    Just auth -> genAuthProviderTypes auth
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
