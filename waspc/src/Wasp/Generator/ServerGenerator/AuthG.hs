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
import Wasp.AppSpec.Valid (doesUserEntityContainField, getApp)
import Wasp.Generator.AuthProviders (emailAuthProvider, gitHubAuthProvider, googleAuthProvider, localAuthProvider)
import qualified Wasp.Generator.AuthProviders.Email as EmailProvider
import qualified Wasp.Generator.AuthProviders.Local as LocalProvider
import qualified Wasp.Generator.AuthProviders.OAuth as OAuthProvider
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
        genAuthMiddleware spec auth,
        genAuthRoutesIndex auth,
        genMeRoute auth,
        genUtils auth,
        genProvidersIndex auth,
        genFileCopy [relfile|auth/providers/types.ts|],
        genFileCopy [relfile|auth/validation.ts|]
      ]
      <++> genIndexTs auth
      <++> genLocalAuth auth
      <++> genOAuthAuth spec auth
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
              "userEntityLower" .= (Util.toLowerFirst userEntityName :: String)
            ]

genAuthMiddleware :: AS.AppSpec -> AS.Auth.Auth -> Generator FileDraft
genAuthMiddleware spec auth = return $ C.mkTmplFdWithDstAndData tmplFile dstFile (Just tmplData)
  where
    -- TODO(martin): In prismaMiddleware.js, we assume that 'username' and 'password' are defined in user entity.
    --   This was promised to us by AppSpec, which has validation checks for this.
    --   Names of these fields are currently hardcoded, and we are not in any way relyin on AppSpec directly here.
    --   In the future we might want to figure out a way to better encode these assumptions, either by
    --   reusing the names for 'username' and 'password' fields by importing them from AppSpec, or smth similar
    --   in that direction.
    authMiddlewareRelToSrc = [relfile|core/auth/prismaMiddleware.js|]
    tmplFile = C.asTmplFile $ [reldir|src|] </> authMiddlewareRelToSrc
    dstFile = C.serverSrcDirInServerRootDir </> C.asServerSrcFile authMiddlewareRelToSrc

    tmplData =
      let userEntityName = AS.refName $ AS.Auth.userEntity auth
          isPasswordOnUserEntity = doesUserEntityContainField spec "password" == Just True
          isUsernameOnUserEntity = doesUserEntityContainField spec "username" == Just True
       in object
            [ "userEntityUpper" .= userEntityName,
              "isUsernameAndPasswordAuthEnabled" .= AS.Auth.isUsernameAndPasswordAuthEnabled auth,
              "isPasswordOnUserEntity" .= isPasswordOnUserEntity,
              "isUsernameOnUserEntity" .= isUsernameOnUserEntity,
              "isEmailAuthEnabled" .= AS.Auth.isEmailAuthEnabled auth
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
