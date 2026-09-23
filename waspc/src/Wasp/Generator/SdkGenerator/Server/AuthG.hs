module Wasp.Generator.SdkGenerator.Server.AuthG
  ( genServerAuth,
  )
where

import Data.Aeson (object, (.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Aeson.Key
import Data.List (sortOn)
import Data.Maybe (isJust)
import StrongPath (Dir', File', Path', Rel, Rel', reldir, relfile, (</>))
import Wasp.AppSpec (AppSpec)
import qualified Wasp.AppSpec as AS
import qualified Wasp.AppSpec.App as AS.App
import qualified Wasp.AppSpec.App.Auth as AS.Auth
import qualified Wasp.AppSpec.App.Db as AS.Db
import qualified Wasp.AppSpec.App.EmailSender as AS.EmailSender
import Wasp.AppSpec.Valid (getApp)
import qualified Wasp.AppSpec.Valid as AS.Valid
import Wasp.Generator.Common (makeJsArrayFromHaskellList)
import qualified Wasp.Generator.DbGenerator.Auth as DbAuth
import Wasp.Generator.FileDraft (FileDraft)
import Wasp.Generator.Monad (Generator)
import Wasp.Generator.SdkGenerator.Auth.HandlerSpec (mkHandlerSpecTmplData)
import Wasp.Generator.SdkGenerator.Common
  ( SdkTemplatesDir,
    genFileCopy,
    mkTmplFdWithData,
  )
import Wasp.Generator.SdkGenerator.JsImport (extImportToAliasedImportJson)
import qualified Wasp.Util as Util
import qualified Wasp.Util.Aeson as Util.Aeson

-- | The scheme registry, the credential issuer, the identity store and the
-- hook dispatch: the scheme-agnostic server auth surface. Every scheme,
-- Wasp's own auth included, is a handler instantiated in the registry.
genServerAuth :: AppSpec -> Generator [FileDraft]
genServerAuth spec =
  case maybeAuth of
    Nothing -> return []
    Just auth ->
      sequence
        [ genFileCopy [relfile|server/core/auth.ts|],
          genFileCopyInServerAuth [relfile|index.ts|],
          genFileCopyInServerAuth [relfile|http.ts|],
          genFileCopy [relfile|server/requestContext.ts|],
          genFileCopyInServerAuth [relfile|handler/types.ts|],
          genSchemesTs spec auth,
          genIssuerTs auth,
          genSingleUseAuthTicketsTs,
          genSessionTs auth,
          genImperativeTs,
          genSessionStoreTs auth,
          genIdentityStoreTs auth,
          genLuciaTs auth,
          genUtils auth,
          genFileCopyInServerAuth [relfile|hooks.ts|],
          genHookDispatchTs auth
        ]
  where
    maybeAuth = AS.App.auth $ snd $ getApp spec

-- | Dispatch for the app-level lifecycle hooks (`auth.hooks`): fired from the
-- SDK's provisioning and credential-issuing choke points, so every scheme is
-- covered and none can skip them.
genHookDispatchTs :: AS.Auth.Auth -> Generator FileDraft
genHookDispatchTs auth =
  return $
    mkTmplFdWithData
      (serverAuthDirInSdkTemplatesDir </> [relfile|hookDispatch.ts|])
      tmplData
  where
    tmplData =
      object
        [ "onBeforeSignupHook" .= extImportToAliasedImportJson "onBeforeSignupHook_ext" (AS.Auth.onBeforeSignup auth),
          "onAfterSignupHook" .= extImportToAliasedImportJson "onAfterSignupHook_ext" (AS.Auth.onAfterSignup auth),
          "onBeforeLoginHook" .= extImportToAliasedImportJson "onBeforeLoginHook_ext" (AS.Auth.onBeforeLogin auth),
          "onAfterLoginHook" .= extImportToAliasedImportJson "onAfterLoginHook_ext" (AS.Auth.onAfterLogin auth),
          "onBeforeLinkHook" .= extImportToAliasedImportJson "onBeforeLinkHook_ext" (AS.Auth.onBeforeLink auth),
          "onAfterLinkHook" .= extImportToAliasedImportJson "onAfterLinkHook_ext" (AS.Auth.onAfterLink auth),
          "mergeUsersFn" .= extImportToAliasedImportJson "mergeUsersFn_ext" (AS.Auth.mergeUsers auth)
        ]

genIdentityStoreTs :: AS.Auth.Auth -> Generator FileDraft
genIdentityStoreTs auth =
  return $
    mkTmplFdWithData
      (serverAuthDirInSdkTemplatesDir </> [relfile|identityStore.ts|])
      tmplData
  where
    tmplData =
      object
        [ "userEntityUpper" .= (userEntityName :: String),
          "userEntityLower" .= (Util.toLowerFirst userEntityName :: String),
          "authEntityUpper" .= (DbAuth.authEntityName :: String),
          "authIdentityEntityLower" .= (Util.toLowerFirst DbAuth.authIdentityEntityName :: String),
          "authFieldOnUserEntityName" .= (DbAuth.authFieldOnUserEntityName :: String),
          "identitiesFieldOnAuthEntityName" .= (DbAuth.identitiesFieldOnAuthEntityName :: String)
        ]
    userEntityName = AS.refName $ AS.Auth.userEntity auth

-- | The Prisma-backed credential store over the injected Session model.
-- Generated unconditionally so the issuer module always typechecks; the model
-- access sits behind the flag.
genSessionStoreTs :: AS.Auth.Auth -> Generator FileDraft
genSessionStoreTs auth =
  return $
    mkTmplFdWithData
      (serverAuthDirInSdkTemplatesDir </> [relfile|sessionStore.ts|])
      tmplData
  where
    tmplData =
      object
        [ "sessionEntityLower" .= (Util.toLowerFirst DbAuth.sessionEntityName :: String),
          "sessionEntityUpper" .= (DbAuth.sessionEntityName :: String),
          "authEntityLower" .= (Util.toLowerFirst DbAuth.authEntityName :: String),
          "isPrismaStoreUsed" .= usesPrismaStore auth
        ]

genLuciaTs :: AS.Auth.Auth -> Generator FileDraft
genLuciaTs auth =
  return $
    mkTmplFdWithData
      (serverAuthDirInSdkTemplatesDir </> [relfile|lucia.ts|])
      tmplData
  where
    tmplData =
      object
        [ "sessionEntityLower" .= (Util.toLowerFirst DbAuth.sessionEntityName :: String),
          "authEntityLower" .= (Util.toLowerFirst DbAuth.authEntityName :: String),
          "userEntityUpper" .= (userEntityName :: String),
          "isPrismaStoreUsed" .= usesPrismaStore auth
        ]

    userEntityName = AS.refName $ AS.Auth.userEntity auth

-- | Single-use auth tickets for navigations: rows in Wasp's own table.
genSingleUseAuthTicketsTs :: Generator FileDraft
genSingleUseAuthTicketsTs =
  return $
    mkTmplFdWithData
      (serverAuthDirInSdkTemplatesDir </> [relfile|singleUseAuthTickets.ts|])
      ( object
          [ "singleUseAuthTicketEntityUpper" .= (DbAuth.singleUseAuthTicketEntityName :: String),
            "singleUseAuthTicketEntityLower" .= (Util.toLowerFirst DbAuth.singleUseAuthTicketEntityName :: String)
          ]
      )

-- | The framework's credential issuer: bearer or cookie transport over a
-- credential store. Backs every inline `credentials: { transport, store }`.
genIssuerTs :: AS.Auth.Auth -> Generator FileDraft
genIssuerTs auth =
  return $
    mkTmplFdWithData
      (serverAuthDirInSdkTemplatesDir </> [relfile|issuer.ts|])
      tmplData
  where
    tmplData =
      object
        [ "authEntityLower" .= (Util.toLowerFirst DbAuth.authEntityName :: String),
          "failureRedirectPath" .= AS.Auth.onAuthFailedRedirectTo auth,
          "isPrismaStoreUsed" .= usesPrismaStore auth
        ]

usesPrismaStore :: AS.Auth.Auth -> Bool
usesPrismaStore auth =
  any
    ( \scheme -> case AS.Auth.inlineCredentials scheme of
        Just (_, AS.Auth.PrismaStore, _, _, _) -> True
        _ -> False
    )
    (AS.Auth.schemes auth)

-- | The scheme registry the app runs on: one entry per declared scheme, in an
-- order where every credentials target is created before the schemes that
-- sign into it. Handler packages are instantiated here (each with the
-- identity store pre-bound to its own name), user-module handlers are
-- imported through virtual user modules.
genSchemesTs :: AppSpec -> AS.Auth.Auth -> Generator FileDraft
genSchemesTs spec auth =
  return $
    mkTmplFdWithData
      (serverAuthDirInSdkTemplatesDir </> [relfile|schemes.ts|])
      tmplData
  where
    tmplData =
      object
        [ "dbProvider" .= prismaDbProviderName,
          "authFieldOnUserEntityName" .= DbAuth.authFieldOnUserEntityName,
          "authIdentityEntityLower" .= Util.toLowerFirst DbAuth.authIdentityEntityName,
          "authEntityLower" .= Util.toLowerFirst DbAuth.authEntityName,
          "userFieldOnAuthEntityName" .= DbAuth.userFieldOnAuthEntityName,
          "userEntityLower" .= Util.toLowerFirst (AS.refName $ AS.Auth.userEntity auth),
          "defaultScheme" .= AS.Auth.defaultScheme auth,
          -- Where a cookie-transport issuer sends a browser navigation that
          -- carries no credential.
          "failureRedirectPath" .= AS.Auth.onAuthFailedRedirectTo auth,
          -- The email-send grant can only be wired when the app has an email
          -- sender; validation guarantees no manifest requests it otherwise.
          "isEmailSenderEnabled" .= isJust maybeEmailSender,
          "defaultFromJson"
            .= maybe "undefined" Util.Aeson.encodeToString (AS.EmailSender.defaultFrom =<< maybeEmailSender),
          "schemes" .= mkSchemesTmplData auth
        ]
    maybeEmailSender = AS.App.emailSender $ snd $ AS.Valid.getApp spec
    prismaDbProviderName :: String
    prismaDbProviderName = case AS.Valid.getValidDbSystem spec of
      AS.Db.PostgreSQL -> "postgresql"
      AS.Db.SQLite -> "sqlite"

-- | Per-scheme template data, in dependency order (credentials targets
-- first). Import aliases carry the scheme's declaration index so that two
-- schemes whose user modules share an export name never collide in one
-- generated file.
mkSchemesTmplData :: AS.Auth.Auth -> [Aeson.Value]
mkSchemesTmplData auth =
  mkSchemeTmplData <$> sortOn (chainDepth . snd) indexedSchemes
  where
    indexedSchemes = zip [0 :: Int ..] (AS.Auth.schemes auth)

    -- How many sign-in hops a scheme is away from a self-issuing one.
    -- Validation guarantees the chain ends, so this terminates.
    chainDepth :: AS.Auth.AuthScheme -> Int
    chainDepth scheme = case AS.Auth.credentialsScheme scheme >>= findScheme of
      Nothing -> 0
      Just target -> 1 + chainDepth target
    findScheme schemeName = lookup schemeName [(AS.Auth.name s, s) | s <- AS.Auth.schemes auth]

    mkSchemeTmplData (idx, scheme) =
      object $
        mkHandlerSpecTmplData ("authSchemeSpecReference_" ++ show idx) scheme.server
          ++ [ "index" .= idx,
               "schemeName" .= scheme.name,
               "handler" .= scheme.handler,
               "isPackage" .= isJust (AS.Auth.serverPackage scheme),
               -- The framework's own issuer handler IS the private issuer built
               -- from the scheme's inline credentials; nothing else to construct.
               "isFrameworkIssuer" .= (AS.Auth.serverPackage scheme == Just frameworkIssuerPackage),
               "serverPackage" .= AS.Auth.serverPackage scheme,
               "handlerModule"
                 .= extImportToAliasedImportJson ("authHandlerModule_" ++ show idx) (AS.Auth.serverModule scheme),
               "serverExportName" .= AS.Auth.serverExportName scheme,
               "userFieldsFromClaims"
                 .= extImportToAliasedImportJson
                   ("authSchemeUserFieldsFromClaims_" ++ show idx)
                   scheme.userFieldsFromClaims,
               -- The manifest's compile-time claims, checked against the runtime
               -- handler object at boot so a wrong manifest fails loudly instead of
               -- generating a surface the handler cannot back.
               "capabilitiesJs" .= makeJsArrayFromHaskellList scheme.capabilities,
               -- The handler runtime's env is narrowed to exactly these names.
               "serverEnvVarNamesJs"
                 .= makeJsArrayFromHaskellList ((.envVarName) <$> AS.Auth.serverEnvVars scheme),
               -- The runtime facets the manifest requested; only these get wired.
               "usesJs" .= makeJsArrayFromHaskellList scheme.uses,
               -- The scheme's providers, by name, as a JS object literal. The
               -- runtime builds one identity store per name and demands the
               -- OAuth data for every provider whose kind is "oauth".
               "providersJs"
                 .= Util.Aeson.encodeToString
                   ( Aeson.object
                       [ Aeson.Key.fromString provider.providerName
                           .= Aeson.object ["kind" .= kind | Just kind <- [provider.providerKind]]
                         | provider <- scheme.providers
                       ]
                   ),
               "hasCredentials" .= isJust scheme.credentials,
               "credentialsScheme" .= AS.Auth.credentialsScheme scheme,
               "inlineCredentials" .= (inlineCredentialsTmplData idx <$> AS.Auth.inlineCredentials scheme)
             ]

    inlineCredentialsTmplData idx (transport, store, ttl, freshFor, slidingRenewal) =
      object
        [ "transport" .= transportName transport,
          "storeKind" .= storeKind store,
          "storeModule"
            .= extImportToAliasedImportJson
              ("authSchemeCredentialStore_" ++ show idx)
              (case store of AS.Auth.CustomStore extImport -> Just extImport; _ -> Nothing),
          "ttl" .= ttl,
          "freshFor" .= freshFor,
          "slidingRenewal" .= slidingRenewal
        ]
    transportName AS.Auth.BearerTransport = "bearer" :: String
    transportName AS.Auth.CookieTransport = "cookie"
    storeKind AS.Auth.PrismaStore = "prisma" :: String
    storeKind AS.Auth.SignedTokenStore = "signed-token"
    storeKind (AS.Auth.CustomStore _) = "custom"

frameworkIssuerPackage :: String
frameworkIssuerPackage = "wasp/server/auth/issuer"

genSessionTs :: AS.Auth.Auth -> Generator FileDraft
genSessionTs auth =
  return $
    mkTmplFdWithData
      (serverAuthDirInSdkTemplatesDir </> [relfile|session.ts|])
      tmplData
  where
    tmplData =
      object
        [ "userEntityUpper" .= userEntityName,
          "userEntityLower" .= Util.toLowerFirst userEntityName,
          "authFieldOnUserEntityName" .= DbAuth.authFieldOnUserEntityName,
          "authIdentityEntityLower" .= Util.toLowerFirst DbAuth.authIdentityEntityName,
          "identitiesFieldOnAuthEntityName" .= DbAuth.identitiesFieldOnAuthEntityName,
          "schemes" .= mkSchemesTmplData auth
        ]
    userEntityName = AS.refName $ AS.Auth.userEntity auth

-- | The imperative auth API (`signIn`, `signOut`, ...) app code calls from
-- `api()` routes.
genImperativeTs :: Generator FileDraft
genImperativeTs =
  return $
    mkTmplFdWithData
      (serverAuthDirInSdkTemplatesDir </> [relfile|imperative.ts|])
      (object ["authIdentityEntityLower" .= (Util.toLowerFirst DbAuth.authIdentityEntityName :: String)])

genUtils :: AS.Auth.Auth -> Generator FileDraft
genUtils auth =
  return $
    mkTmplFdWithData
      (serverAuthDirInSdkTemplatesDir </> [relfile|utils.ts|])
      tmplData
  where
    tmplData =
      object
        [ "userEntityUpper" .= (userEntityName :: String),
          "userEntityLower" .= (Util.toLowerFirst userEntityName :: String),
          "authEntityUpper" .= (DbAuth.authEntityName :: String),
          "authEntityLower" .= (Util.toLowerFirst DbAuth.authEntityName :: String),
          "userFieldOnAuthEntityName" .= (DbAuth.userFieldOnAuthEntityName :: String),
          "authIdentityEntityUpper" .= (DbAuth.authIdentityEntityName :: String),
          "authIdentityEntityLower" .= (Util.toLowerFirst DbAuth.authIdentityEntityName :: String),
          "authFieldOnUserEntityName" .= (DbAuth.authFieldOnUserEntityName :: String),
          "identitiesFieldOnAuthEntityName" .= (DbAuth.identitiesFieldOnAuthEntityName :: String),
          "failureRedirectPath" .= AS.Auth.onAuthFailedRedirectTo auth
        ]
    userEntityName = AS.refName $ AS.Auth.userEntity auth

serverAuthDirInSdkTemplatesDir :: Path' (Rel SdkTemplatesDir) Dir'
serverAuthDirInSdkTemplatesDir = [reldir|server/auth|]

genFileCopyInServerAuth :: Path' Rel' File' -> Generator FileDraft
genFileCopyInServerAuth =
  genFileCopy . (serverAuthDirInSdkTemplatesDir </>)
