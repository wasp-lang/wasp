module Wasp.Generator.ServerGenerator.AuthG
  ( genAuth,
  )
where

import Data.Aeson (object, (.=))
import Data.Maybe (fromMaybe)
import StrongPath
  ( File',
    Path',
    Rel,
    reldir,
    reldirP,
    relfile,
    (</>),
  )
import qualified StrongPath as SP
import Wasp.AppSpec (AppSpec)
import qualified Wasp.AppSpec as AS
import qualified Wasp.AppSpec.App as AS.App
import qualified Wasp.AppSpec.App.Auth as AS.Auth
import Wasp.AppSpec.Valid (getApp)
import Wasp.Generator.AuthProviders (emailAuthProvider, gitHubAuthProvider, googleAuthProvider, localAuthProvider)
import qualified Wasp.Generator.AuthProviders.Email as EmailProvider
import qualified Wasp.Generator.AuthProviders.Local as LocalProvider
import qualified Wasp.Generator.AuthProviders.OAuth as OAuthProvider
import qualified Wasp.Generator.DbGenerator.Auth as DbAuth
import Wasp.Generator.FileDraft (FileDraft)
import Wasp.Generator.Monad (Generator)
import Wasp.Generator.ServerGenerator.Auth.EmailAuthG (genEmailAuth)
import Wasp.Generator.ServerGenerator.Auth.LocalAuthG (genLocalAuth)
import Wasp.Generator.ServerGenerator.Auth.OAuthAuthG (genOAuthAuth)
import qualified Wasp.Generator.ServerGenerator.Common as C
import Wasp.Generator.ServerGenerator.JsImport (extImportToImportJson)
import Wasp.Util ((<++>))
import qualified Wasp.Util as Util

genAuth :: AppSpec -> Generator [FileDraft]
genAuth spec = case maybeAuth of
  Just auth ->
    sequence
      [ genCoreAuth auth,
        genAuthRoutesIndex auth,
        genMeRoute auth,
        genUtils auth,
        genProvidersIndex auth,
        genProvidersTypes auth,
        genFileCopy [relfile|auth/validation.ts|]
      ]
      <++> genIndexTs auth
      <++> genLocalAuth auth
      <++> genOAuthAuth auth
      <++> genEmailAuth spec auth
  Nothing -> return []
  where
    maybeAuth = AS.App.auth $ snd $ getApp spec
    genFileCopy = return . C.mkSrcTmplFd

-- | Generates core/auth file which contains auth middleware and createUser() function.
genCoreAuth :: AS.Auth.Auth -> Generator FileDraft
genCoreAuth auth = return $ C.mkTmplFdWithDstAndData tmplFile dstFile (Just tmplData)
  where
    coreAuthRelToSrc = [relfile|core/auth.js|]
    tmplFile = C.asTmplFile $ [reldir|src|] </> coreAuthRelToSrc
    dstFile = C.serverSrcDirInServerRootDir </> C.asServerSrcFile coreAuthRelToSrc

    tmplData =
      let userEntityName = AS.refName $ AS.Auth.userEntity auth
       in object
            [ "userEntityUpper" .= (userEntityName :: String),
              "userEntityLower" .= (Util.toLowerFirst userEntityName :: String),
              "authFieldOnUserEntityName" .= (DbAuth.authFieldOnUserEntityName :: String),
              "identitiesFieldOnAuthEntityName" .= (DbAuth.identitiesFieldOnAuthEntityName :: String)
            ]

genAuthRoutesIndex :: AS.Auth.Auth -> Generator FileDraft
genAuthRoutesIndex auth = return $ C.mkTmplFdWithDstAndData tmplFile dstFile (Just tmplData)
  where
    tmplFile = C.srcDirInServerTemplatesDir </> SP.castRel authIndexFileInSrcDir
    dstFile = C.serverSrcDirInServerRootDir </> authIndexFileInSrcDir
    tmplData =
      object ["isExternalAuthEnabled" .= AS.Auth.isExternalAuthEnabled auth]

    authIndexFileInSrcDir :: Path' (Rel C.ServerSrcDir) File'
    authIndexFileInSrcDir = [relfile|routes/auth/index.js|]

genMeRoute :: AS.Auth.Auth -> Generator FileDraft
genMeRoute auth = return $ C.mkTmplFdWithDstAndData tmplFile dstFile (Just tmplData)
  where
    meRouteRelToSrc = [relfile|routes/auth/me.js|]
    tmplFile = C.asTmplFile $ [reldir|src|] </> meRouteRelToSrc
    dstFile = C.serverSrcDirInServerRootDir </> C.asServerSrcFile meRouteRelToSrc

    tmplData = object ["userEntityLower" .= (Util.toLowerFirst (AS.refName $ AS.Auth.userEntity auth) :: String)]

genUtils :: AS.Auth.Auth -> Generator FileDraft
genUtils auth = return $ C.mkTmplFdWithDstAndData tmplFile dstFile (Just tmplData)
  where
    userEntityName = AS.refName $ AS.Auth.userEntity auth
    tmplFile = C.srcDirInServerTemplatesDir </> SP.castRel utilsFileInSrcDir
    dstFile = C.serverSrcDirInServerRootDir </> utilsFileInSrcDir
    tmplData =
      object
        [ "userEntityUpper" .= (userEntityName :: String),
          "userEntityLower" .= (Util.toLowerFirst userEntityName :: String),
          "authEntityUpper" .= (DbAuth.authEntityName :: String),
          "authEntityLower" .= (Util.toLowerFirst DbAuth.authEntityName :: String),
          "userFieldOnAuthEntityName" .= (DbAuth.userFieldOnAuthEntityName :: String),
          "authIdentityEntityUpper" .= (DbAuth.authIdentityEntityName :: String),
          "authIdentityEntityLower" .= (Util.toLowerFirst DbAuth.authIdentityEntityName :: String),
          "identitiesFieldOnAuthEntityName" .= (DbAuth.identitiesFieldOnAuthEntityName :: String),
          "failureRedirectPath" .= AS.Auth.onAuthFailedRedirectTo auth,
          "successRedirectPath" .= getOnAuthSucceededRedirectToOrDefault auth,
          "additionalSignupFields" .= extImportToImportJson [reldirP|../|] additionalSignupFields
        ]

    utilsFileInSrcDir :: Path' (Rel C.ServerSrcDir) File'
    utilsFileInSrcDir = [relfile|auth/utils.ts|]

    additionalSignupFields = AS.Auth.signup auth >>= AS.Auth.additionalFields

genIndexTs :: AS.Auth.Auth -> Generator [FileDraft]
genIndexTs auth =
  return $
    if isEmailAuthEnabled || isLocalAuthEnabled
      then [C.mkTmplFdWithData [relfile|src/auth/index.ts|] (Just tmplData)]
      else []
  where
    tmplData =
      object
        [ "isEmailAuthEnabled" .= isEmailAuthEnabled,
          "isLocalAuthEnabled" .= isLocalAuthEnabled
        ]
    isEmailAuthEnabled = AS.Auth.isEmailAuthEnabled auth
    isLocalAuthEnabled = AS.Auth.isUsernameAndPasswordAuthEnabled auth

getOnAuthSucceededRedirectToOrDefault :: AS.Auth.Auth -> String
getOnAuthSucceededRedirectToOrDefault auth = fromMaybe "/" (AS.Auth.onAuthSucceededRedirectTo auth)

genProvidersIndex :: AS.Auth.Auth -> Generator FileDraft
genProvidersIndex auth = return $ C.mkTmplFdWithData [relfile|src/auth/providers/index.ts|] (Just tmplData)
  where
    tmplData = object ["enabledProviderIds" .= (enabledProviderIds :: [String])]

    enabledProviderIds =
      concat
        [ [OAuthProvider.providerId gitHubAuthProvider | AS.Auth.isGitHubAuthEnabled auth],
          [OAuthProvider.providerId googleAuthProvider | AS.Auth.isGoogleAuthEnabled auth],
          [LocalProvider.providerId localAuthProvider | AS.Auth.isUsernameAndPasswordAuthEnabled auth],
          [EmailProvider.providerId emailAuthProvider | AS.Auth.isEmailAuthEnabled auth]
        ]

genProvidersTypes :: AS.Auth.Auth -> Generator FileDraft
genProvidersTypes auth = return $ C.mkTmplFdWithData [relfile|src/auth/providers/types.ts|] (Just tmplData)
  where
    userEntityName = AS.refName $ AS.Auth.userEntity auth

    tmplData = object ["userEntityUpper" .= (userEntityName :: String)]
