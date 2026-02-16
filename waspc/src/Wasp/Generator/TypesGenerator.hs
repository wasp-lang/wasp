module Wasp.Generator.TypesGenerator
  ( genTypes,
  )
where

import Data.Aeson (object, (.=))
import StrongPath (reldir, relfile, (</>))
import Wasp.AppSpec (AppSpec)
import qualified Wasp.AppSpec.App as AS.App
import qualified Wasp.AppSpec.App.Auth as AS.Auth
import qualified Wasp.AppSpec.App.Db as AS.Db
import qualified Wasp.AppSpec.App.WebSocket as AS.App.WS
import Wasp.AppSpec.Valid (getApp)
import Wasp.Generator.FileDraft (FileDraft)
import Wasp.Generator.Monad (Generator)
import Wasp.Generator.SdkGenerator.JsImport (extImportToImportJson)
import Wasp.Generator.TypesGenerator.Common (mkTmplFdWithData)
import qualified Wasp.Generator.WebSocket as AS.WS
import Wasp.Util ((<++>))

genTypes :: AppSpec -> Generator [FileDraft]
genTypes spec =
  genDbTypes spec
    <++> case maybeAuth of
      Nothing -> return []
      Just auth -> genAuthProviderTypes auth
    <++> genWebSocketTypes spec
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
