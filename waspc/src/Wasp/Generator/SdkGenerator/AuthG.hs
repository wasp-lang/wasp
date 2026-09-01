module Wasp.Generator.SdkGenerator.AuthG
  ( genAuth,
  )
where

import Data.Aeson (object, (.=))
import Data.Maybe (isJust)
import StrongPath (Dir', File', Path', Rel, Rel', reldir, relfile, (</>))
import Wasp.AppSpec (AppSpec)
import qualified Wasp.AppSpec as AS
import qualified Wasp.AppSpec.App as AS.App
import qualified Wasp.AppSpec.App.Auth as AS.Auth
import Wasp.AppSpec.Valid (getApp)
import qualified Wasp.Generator.AuthProviders as AuthProviders
import Wasp.Generator.Common (makeJsArrayFromHaskellList)
import qualified Wasp.Generator.DbGenerator.Auth as DbAuth
import Wasp.Generator.FileDraft (FileDraft)
import Wasp.Generator.Monad (Generator)
import Wasp.Generator.SdkGenerator.Common
  ( SdkTemplatesDir,
    genFileCopy,
    mkTmplFdWithData,
  )
import Wasp.Util ((<++>))

genAuth :: AppSpec -> Generator [FileDraft]
genAuth spec =
  case maybeAuth of
    Nothing -> return []
    Just auth ->
      -- shared stuff
      sequence
        [ genUserTs auth,
          genAuthProviderIdentity auth,
          genFileCopyInAuth [relfile|providerData.ts|],
          genFileCopyInAuth [relfile|validation.ts|],
          genIndexTs auth,
          genProvidersTypes auth,
          genProvdersIndex auth
        ]
        -- client stuff. Wasp's own auth UI, flows and actions live in the
        -- @wasp.sh/auth lib; the SDK only re-exports them.
        <++> sequence
          [ genFileCopyInAuth [relfile|types.ts|],
            genLogoutTs auth,
            genUseAuth auth
          ]
  where
    maybeAuth = AS.App.auth $ snd $ getApp spec

-- | The one module that always answers "which auth providers is this app on":
-- literal ids the type system narrows, so provider-specific code can be
-- guarded at compile time.
genAuthProviderIdentity :: AS.Auth.Auth -> Generator FileDraft
genAuthProviderIdentity auth =
  return $
    mkTmplFdWithData
      (authDirInSdkTemplatesDir </> [relfile|provider.ts|])
      tmplData
  where
    tmplData =
      object
        [ "authProviders" .= (providerTmplData <$> AS.Auth.providers auth),
          "isWaspAuthProviderUsed" .= AS.Auth.isWaspAuthProviderUsed auth
        ]
    providerTmplData authProvider =
      object
        [ "providerId" .= AS.Auth.authProviderId authProvider,
          "capabilitiesJs" .= makeJsArrayFromHaskellList (capabilitiesOf authProvider)
        ]
    -- Wasp's own auth can mint and revoke sessions server-side.
    capabilitiesOf (AS.Auth.WaspAuthProvider _) = ["issue-sessions", "session-revocation"]
    capabilitiesOf (AS.Auth.ExternalAuthProvider extProvider) = extProvider.capabilities

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

genLogoutTs :: AS.Auth.Auth -> Generator FileDraft
genLogoutTs _auth =
  genFileCopyInAuth [relfile|logout.ts|]

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
          "identitiesFieldOnAuthEntityName" .= DbAuth.identitiesFieldOnAuthEntityName,
          "enabledProviders" .= AuthProviders.getEnabledAuthProvidersJson auth,
          "isWaspAuthProviderUsed" .= AS.Auth.isWaspAuthProviderUsed auth
        ]
    userEntityName = AS.refName $ AS.Auth.userEntity auth

genIndexTs :: AS.Auth.Auth -> Generator FileDraft
genIndexTs auth =
  return $
    mkTmplFdWithData
      (authDirInSdkTemplatesDir </> [relfile|index.ts|])
      tmplData
  where
    tmplData =
      object
        [ "isEmailAuthEnabled" .= isEmailAuthEnabled,
          "isLocalAuthEnabled" .= isLocalAuthEnabled
        ]
    isEmailAuthEnabled = AS.Auth.isEmailAuthEnabled auth
    isLocalAuthEnabled = AS.Auth.isUsernameAndPasswordAuthEnabled auth

genProvdersIndex :: AS.Auth.Auth -> Generator FileDraft
genProvdersIndex auth =
  return $
    mkTmplFdWithData
      (authDirInSdkTemplatesDir </> [relfile|providers/index.ts|])
      tmplData
  where
    tmplData =
      object
        [ "isEmailUserSignupFieldsDefined" .= isJust emailUserSignupFields,
          "isUsernameAndPasswordUserSignupFieldsDefined" .= isJust usernameAndPasswordUserSignupFields
        ]
    emailUserSignupFields = AS.Auth.email authMethods >>= AS.Auth.userSignupFieldsForEmailAuth
    usernameAndPasswordUserSignupFields = AS.Auth.usernameAndPassword authMethods >>= AS.Auth.userSignupFieldsForUsernameAuth
    authMethods = AS.Auth.methods auth

genProvidersTypes :: AS.Auth.Auth -> Generator FileDraft
genProvidersTypes auth =
  return $
    mkTmplFdWithData
      (authDirInSdkTemplatesDir </> [relfile|providers/types.ts|])
      tmplData
  where
    tmplData =
      object
        [ "userEntityUpper" .= (userEntityName :: String),
          "isEmailUserSignupFieldsDefined" .= isJust emailUserSignupFields,
          "isUsernameAndPasswordUserSignupFieldsDefined" .= isJust usernameAndPasswordUserSignupFields
        ]
    userEntityName = AS.refName $ AS.Auth.userEntity auth
    emailUserSignupFields = AS.Auth.email authMethods >>= AS.Auth.userSignupFieldsForEmailAuth
    usernameAndPasswordUserSignupFields = AS.Auth.usernameAndPassword authMethods >>= AS.Auth.userSignupFieldsForUsernameAuth
    authMethods = AS.Auth.methods auth

authDirInSdkTemplatesDir :: Path' (Rel SdkTemplatesDir) Dir'
authDirInSdkTemplatesDir = [reldir|auth|]

genFileCopyInAuth :: Path' Rel' File' -> Generator FileDraft
genFileCopyInAuth =
  genFileCopy . (authDirInSdkTemplatesDir </>)
