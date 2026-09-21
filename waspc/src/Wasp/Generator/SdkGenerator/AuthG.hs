module Wasp.Generator.SdkGenerator.AuthG
  ( genAuth,
  )
where

import Data.Aeson (object, (.=))
import StrongPath (Dir', File', Path', Rel, Rel', reldir, relfile, (</>))
import Wasp.AppSpec (AppSpec)
import qualified Wasp.AppSpec as AS
import qualified Wasp.AppSpec.App as AS.App
import qualified Wasp.AppSpec.App.Auth as AS.Auth
import Wasp.AppSpec.Valid (getApp)
import Wasp.Generator.Common (makeJsArrayFromHaskellList)
import qualified Wasp.Generator.DbGenerator.Auth as DbAuth
import Wasp.Generator.FileDraft (FileDraft)
import Wasp.Generator.Monad (Generator)
import Wasp.Generator.SdkGenerator.Common
  ( SdkTemplatesDir,
    genFileCopy,
    mkTmplFdWithData,
  )

-- | The provider-agnostic auth surface: the user shape, the provider identity
-- module, the identity row helpers, signup-field typing, and the client's
-- @useAuth@/@logout@. Everything method-shaped (forms, validators, method
-- identity helpers) lives in the auth provider packages.
genAuth :: AppSpec -> Generator [FileDraft]
genAuth spec =
  case maybeAuth of
    Nothing -> return []
    Just auth ->
      sequence
        [ genUserTs auth,
          genAuthSchemeIdentity auth,
          genFileCopyInAuth [relfile|providerData.ts|],
          genFileCopyInAuth [relfile|index.ts|],
          genProvidersTypes auth,
          genFileCopyInAuth [relfile|providers/index.ts|],
          genFileCopyInAuth [relfile|types.ts|],
          genFileCopyInAuth [relfile|logout.ts|],
          genFileCopyInAuth [relfile|handlerSpec.ts|],
          genUseAuth auth
        ]
  where
    maybeAuth = AS.App.auth $ snd $ getApp spec

-- | The one module that always answers "which auth schemes is this app on":
-- literal names the type system narrows, so scheme-specific code can be
-- guarded at compile time.
genAuthSchemeIdentity :: AS.Auth.Auth -> Generator FileDraft
genAuthSchemeIdentity auth =
  return $
    mkTmplFdWithData
      (authDirInSdkTemplatesDir </> [relfile|scheme.ts|])
      tmplData
  where
    tmplData =
      object
        [ "authSchemes" .= (schemeTmplData <$> AS.Auth.schemes auth),
          "defaultScheme" .= AS.Auth.defaultScheme auth
        ]
    schemeTmplData scheme =
      object
        [ "schemeName" .= AS.Auth.name scheme,
          "capabilitiesJs" .= makeJsArrayFromHaskellList (AS.Auth.capabilities scheme)
        ]

-- | Generates React hook that Wasp developer can use in a component to get
--   access to the currently logged in user (and check whether user is logged in
--   ot not).
genUseAuth :: AS.Auth.Auth -> Generator FileDraft
genUseAuth auth =
  return $
    mkTmplFdWithData
      (authDirInSdkTemplatesDir </> [relfile|useAuth.ts|])
      tmplData
  where
    tmplData = object ["entitiesGetMeDependsOn" .= makeJsArrayFromHaskellList [userEntityName]]
    userEntityName = AS.refName $ AS.Auth.userEntity auth

genUserTs :: AS.Auth.Auth -> Generator FileDraft
genUserTs auth =
  return $
    mkTmplFdWithData
      (authDirInSdkTemplatesDir </> [relfile|user.ts|])
      tmplData
  where
    tmplData =
      object
        [ "userEntityName" .= userEntityName,
          "authEntityName" .= DbAuth.authEntityName,
          "authFieldOnUserEntityName" .= DbAuth.authFieldOnUserEntityName,
          "authIdentityEntityName" .= DbAuth.authIdentityEntityName,
          "identitiesFieldOnAuthEntityName" .= DbAuth.identitiesFieldOnAuthEntityName
        ]
    userEntityName = AS.refName $ AS.Auth.userEntity auth

genProvidersTypes :: AS.Auth.Auth -> Generator FileDraft
genProvidersTypes auth =
  return $
    mkTmplFdWithData
      (authDirInSdkTemplatesDir </> [relfile|providers/types.ts|])
      tmplData
  where
    tmplData = object ["userEntityUpper" .= (userEntityName :: String)]
    userEntityName = AS.refName $ AS.Auth.userEntity auth

authDirInSdkTemplatesDir :: Path' (Rel SdkTemplatesDir) Dir'
authDirInSdkTemplatesDir = [reldir|auth|]

genFileCopyInAuth :: Path' Rel' File' -> Generator FileDraft
genFileCopyInAuth =
  genFileCopy . (authDirInSdkTemplatesDir </>)
