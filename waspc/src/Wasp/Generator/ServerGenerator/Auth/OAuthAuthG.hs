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
import qualified Wasp.AppSpec as AS
import qualified Wasp.AppSpec.App.Auth as AS.Auth
import Wasp.Generator.AuthProviders
  ( discordAuthProvider,
    gitHubAuthProvider,
    googleAuthProvider,
    keycloakAuthProvider,
    microsoftEntraAuthProvider,
    slackAuthProvider,
  )
import Wasp.Generator.AuthProviders.OAuth (OAuthAuthProvider)
import qualified Wasp.Generator.AuthProviders.OAuth as OAuth
import qualified Wasp.Generator.DbGenerator.Auth as DbAuth
import Wasp.Generator.FileDraft (FileDraft)
import Wasp.Generator.Monad (Generator)
import Wasp.Generator.ServerGenerator.Common (ServerTemplatesSrcDir)
import qualified Wasp.Generator.ServerGenerator.Common as C
import Wasp.Generator.ServerGenerator.JsImport (extImportToImportJson)
import Wasp.Util ((<++>))
import qualified Wasp.Util as Util

genOAuthAuth :: AS.Auth.Auth -> Generator [FileDraft]
genOAuthAuth auth
  | AS.Auth.isExternalAuthEnabled auth =
      genOAuthHelpers auth
        <++> genOAuthProvider slackAuthProvider (AS.Auth.slack . AS.Auth.methods $ auth)
        <++> genOAuthProvider discordAuthProvider (AS.Auth.discord . AS.Auth.methods $ auth)
        <++> genOAuthProvider googleAuthProvider (AS.Auth.google . AS.Auth.methods $ auth)
        <++> genOAuthProvider keycloakAuthProvider (AS.Auth.keycloak . AS.Auth.methods $ auth)
        <++> genOAuthProvider gitHubAuthProvider (AS.Auth.gitHub . AS.Auth.methods $ auth)
        <++> genOAuthProvider microsoftEntraAuthProvider (AS.Auth.microsoftEntra . AS.Auth.methods $ auth)
  | otherwise = return []

genOAuthHelpers :: AS.Auth.Auth -> Generator [FileDraft]
genOAuthHelpers auth =
  sequence
    [ genTypes auth,
      genUser,
      return $ C.mkSrcTmplFd [relfile|auth/providers/oauth/handler.ts|],
      return $ C.mkSrcTmplFd [relfile|auth/providers/oauth/state.ts|],
      return $ C.mkSrcTmplFd [relfile|auth/providers/oauth/cookies.ts|],
      return $ C.mkSrcTmplFd [relfile|auth/providers/oauth/config.ts|],
      return $ C.mkSrcTmplFd [relfile|auth/providers/oauth/oneTimeCode.ts|]
    ]

genUser :: Generator FileDraft
genUser = return $ C.mkTmplFdWithData tmplFile (Just tmplData)
  where
    tmplFile = C.srcDirInServerTemplatesDir </> [relfile|auth/providers/oauth/user.ts|]
    tmplData =
      object
        [ "authEntityUpper" .= (DbAuth.authEntityName :: String),
          "authIdentityEntityLower" .= (Util.toLowerFirst DbAuth.authIdentityEntityName :: String),
          "authFieldOnAuthIdentityEntityName" .= (DbAuth.authFieldOnAuthIdentityEntityName :: String),
          "userFieldOnAuthEntityName" .= (DbAuth.userFieldOnAuthEntityName :: String)
        ]

genTypes :: AS.Auth.Auth -> Generator FileDraft
genTypes auth = return $ C.mkTmplFdWithData tmplFile (Just tmplData)
  where
    tmplFile = C.srcDirInServerTemplatesDir </> [relfile|auth/providers/oauth/types.ts|]
    tmplData = object ["userEntityName" .= userEntityName]
    userEntityName = AS.refName $ AS.Auth.userEntity auth

genOAuthProvider ::
  OAuthAuthProvider ->
  Maybe AS.Auth.ExternalAuthConfig ->
  Generator [FileDraft]
genOAuthProvider provider maybeUserConfig
  | isJust maybeUserConfig =
      sequence
        [ genOAuthConfig provider maybeUserConfig $ [reldir|auth/providers/config|] </> providerTsFile
        ]
  | otherwise = return []
  where
    providerTsFile :: Path' (Rel ()) File'
    providerTsFile = fromJust $ SP.parseRelFile $ providerId ++ ".ts"

    providerId = OAuth.providerId provider

-- Used to generate the specific provider config based on the generic oauth.ts file.
-- The config receives everything: auth info, npm packages, user defined imports and env variables.
-- It's all in one config file.
genOAuthConfig ::
  OAuthAuthProvider ->
  Maybe AS.Auth.ExternalAuthConfig ->
  Path' (Rel ServerTemplatesSrcDir) File' ->
  Generator FileDraft
genOAuthConfig provider maybeUserConfig pathToConfigTmpl = return $ C.mkTmplFdWithData tmplFile (Just tmplData)
  where
    tmplFile = C.srcDirInServerTemplatesDir </> pathToConfigTmpl
    tmplData =
      object
        [ "providerId" .= OAuth.providerId provider,
          "displayName" .= OAuth.displayName provider,
          "requiredScopes" .= OAuth.scopeStr provider,
          "configFn" .= extImportToImportJson relPathFromAuthConfigToServerSrcDir maybeConfigFn,
          "userSignupFields" .= extImportToImportJson relPathFromAuthConfigToServerSrcDir maybeUserSignupFields
        ]
    maybeConfigFn = AS.Auth.configFn =<< maybeUserConfig
    maybeUserSignupFields = AS.Auth.userSignupFieldsForExternalAuth =<< maybeUserConfig

    relPathFromAuthConfigToServerSrcDir :: Path Posix (Rel importLocation) (Dir C.ServerSrcDir)
    relPathFromAuthConfigToServerSrcDir = [reldirP|../../../|]
