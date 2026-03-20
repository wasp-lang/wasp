module Wasp.Generator.ServerGenerator.Auth.OAuthAuthG
  ( genOAuthAuth,
  )
where

import Data.Aeson (object, (.=))
import Data.Maybe (fromJust)
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
import Wasp.Generator.Auth.OAuthGen (OAuthGenContext (..), genForEachEnabledOAuth)
import Wasp.Generator.Auth.Provider
  ( OAuthProviderSpec (..),
    isOAuthEnabled,
    oauthProviderScopeStr,
  )
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
  | isOAuthEnabled auth =
      genOAuthHelpers auth
        <++> genForEachEnabledOAuth auth genSingleProvider
  | otherwise = return []

genSingleProvider :: OAuthGenContext -> Generator [FileDraft]
genSingleProvider ctx =
  sequence
    [ genOAuthConfig (oAuthSpec ctx) (oAuthUserConfig ctx) $
        [reldir|auth/providers/config|] </> providerTsFile
    ]
  where
    providerTsFile :: Path' (Rel ()) File'
    providerTsFile = fromJust $ SP.parseRelFile $ slug (oAuthSpec ctx) ++ ".ts"

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

genOAuthConfig ::
  OAuthProviderSpec ->
  AS.Auth.ExternalAuthConfig ->
  Path' (Rel ServerTemplatesSrcDir) File' ->
  Generator FileDraft
genOAuthConfig spec config pathToConfigTmpl = return $ C.mkTmplFdWithData tmplFile (Just tmplData)
  where
    tmplFile = C.srcDirInServerTemplatesDir </> pathToConfigTmpl
    tmplData =
      object
        [ "providerId" .= slug spec,
          "displayName" .= displayName spec,
          "requiredScopes" .= oauthProviderScopeStr spec,
          "configFn" .= extImportToImportJson relPathFromAuthConfigToServerSrcDir (AS.Auth.configFn config),
          "userSignupFields" .= extImportToImportJson relPathFromAuthConfigToServerSrcDir (AS.Auth.userSignupFieldsForExternalAuth config)
        ]

    relPathFromAuthConfigToServerSrcDir :: Path Posix (Rel importLocation) (Dir C.ServerSrcDir)
    relPathFromAuthConfigToServerSrcDir = [reldirP|../../../|]
