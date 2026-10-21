module Wasp.Generator.ServerGenerator.Auth.OAuthAuthG
  ( genOAuthAuth,
  )
where

import Data.Aeson (object, (.=))
import Data.Maybe (fromJust, isJust)
import StrongPath
  ( Dir,
    File',
    Path,
    Path',
    Posix,
    Rel,
    reldir,
    reldirP,
    relfile,
    (</>),
  )
import qualified StrongPath as SP
import qualified Wasp.AppSpec.App.Auth as AS.Auth
import Wasp.Generator.AuthProviders
  ( discordAuthProvider,
    gitHubAuthProvider,
    googleAuthProvider,
    keycloakAuthProvider,
    microsoftAuthProvider,
    slackAuthProvider,
  )
import Wasp.Generator.AuthProviders.OAuth (OAuthAuthProvider)
import qualified Wasp.Generator.AuthProviders.OAuth as OAuth
import Wasp.Generator.FileDraft (FileDraft)
import Wasp.Generator.Monad (Generator)
import qualified Wasp.Generator.ServerGenerator.Common as C
import Wasp.Generator.ServerGenerator.JsImport (extImportToImportJson)
import Wasp.Util ((<++>))

genOAuthAuth :: AS.Auth.Auth -> Generator [FileDraft]
genOAuthAuth auth
  | AS.Auth.isExternalAuthEnabled auth =
      genOAuthHelpers
        <++> genOAuthProvider slackAuthProvider (AS.Auth.slack . AS.Auth.methods $ auth)
        <++> genOAuthProvider discordAuthProvider (AS.Auth.discord . AS.Auth.methods $ auth)
        <++> genOAuthProvider googleAuthProvider (AS.Auth.google . AS.Auth.methods $ auth)
        <++> genOAuthProvider keycloakAuthProvider (AS.Auth.keycloak . AS.Auth.methods $ auth)
        <++> genOAuthProvider gitHubAuthProvider (AS.Auth.gitHub . AS.Auth.methods $ auth)
        <++> genOAuthProvider microsoftAuthProvider (AS.Auth.microsoft . AS.Auth.methods $ auth)
  | otherwise = return []

genOAuthHelpers :: Generator [FileDraft]
genOAuthHelpers =
  return
    [ C.mkSrcTmplFd [relfile|auth/providers/oauth/dependencies.ts|]
    ]

genOAuthProvider ::
  OAuthAuthProvider ->
  Maybe AS.Auth.ExternalAuthConfig ->
  Generator [FileDraft]
genOAuthProvider provider maybeUserConfig
  | isJust maybeUserConfig = sequence [genOAuthConfig provider maybeUserConfig]
  | otherwise = return []

-- Generates the specific provider config from the generic `_oauth.ts` template.
-- The config receives everything: auth info, user defined imports and env variables.
-- It's all in one config file.
genOAuthConfig ::
  OAuthAuthProvider ->
  Maybe AS.Auth.ExternalAuthConfig ->
  Generator FileDraft
genOAuthConfig provider maybeUserConfig = return $ C.mkTmplFdWithDstAndData tmplFile dstFile (Just tmplData)
  where
    tmplFile = C.srcDirInServerTemplatesDir </> [relfile|auth/providers/config/_oauth.ts|]
    dstFile = C.serverSrcDirInServerRootDir </> [reldir|auth/providers/config|] </> providerTsFile

    providerTsFile :: Path' (Rel ()) File'
    providerTsFile = fromJust $ SP.parseRelFile $ providerId ++ ".ts"

    providerId = OAuth.providerId provider

    tmplData =
      object
        [ "providerId" .= providerId,
          "createProviderFunction" .= ("create" ++ OAuth.displayName provider ++ "AuthProvider"),
          "isKeycloak" .= (provider `isSameProviderAs` keycloakAuthProvider),
          "requiredScopes" .= OAuth.scopeStr provider,
          "configFn" .= extImportToImportJson relPathFromAuthConfigToServerSrcDir maybeConfigFn,
          "userSignupFields" .= extImportToImportJson relPathFromAuthConfigToServerSrcDir maybeUserSignupFields
        ]
    maybeConfigFn = AS.Auth.configFn =<< maybeUserConfig
    maybeUserSignupFields = AS.Auth.userSignupFieldsForExternalAuth =<< maybeUserConfig

    isSameProviderAs :: OAuthAuthProvider -> OAuthAuthProvider -> Bool
    isSameProviderAs a b = OAuth.providerId a == OAuth.providerId b

    relPathFromAuthConfigToServerSrcDir :: Path Posix (Rel importLocation) (Dir C.ServerSrcDir)
    relPathFromAuthConfigToServerSrcDir = [reldirP|../../../|]
