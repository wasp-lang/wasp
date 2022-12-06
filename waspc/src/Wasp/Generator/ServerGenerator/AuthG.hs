module Wasp.Generator.ServerGenerator.AuthG
  ( genAuth,
  )
where

import Data.Aeson (object, (.=))
import qualified Data.Aeson as Aeson
import Data.Maybe (fromJust, fromMaybe, isJust)
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
import Wasp.AppSpec (AppSpec)
import qualified Wasp.AppSpec as AS
import qualified Wasp.AppSpec.App as AS.App
import qualified Wasp.AppSpec.App.Auth as AS.Auth
import Wasp.AppSpec.Valid (getApp)
import Wasp.Generator.ExternalCodeGenerator.Common (GeneratedExternalCodeDir)
import Wasp.Generator.FileDraft (FileDraft)
import Wasp.Generator.JsImport (getJsImportDetailsForExtFnImport)
import Wasp.Generator.Monad (Generator)
import qualified Wasp.Generator.ServerGenerator.Common as C
import Wasp.Generator.ServerGenerator.ExternalCodeGenerator (extServerCodeDirInServerSrcDir)
import Wasp.Util ((<++>))
import qualified Wasp.Util as Util

genAuth :: AppSpec -> Generator [FileDraft]
genAuth spec = case maybeAuth of
  Just auth ->
    sequence
      [ genCoreAuth auth,
        genAuthMiddleware auth,
        -- Auth routes
        genAuthRoutesIndex auth,
        genLoginRoute auth,
        genSignupRoute auth,
        genMeRoute auth,
        genUtilsJs auth
      ]
      <++> genPassportAuth auth
  Nothing -> return []
  where
    maybeAuth = AS.App.auth $ snd $ getApp spec

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

genAuthMiddleware :: AS.Auth.Auth -> Generator FileDraft
genAuthMiddleware auth = return $ C.mkTmplFdWithDstAndData tmplFile dstFile (Just tmplData)
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
       in object
            [ "userEntityUpper" .= (userEntityName :: String)
            ]

genAuthRoutesIndex :: AS.Auth.Auth -> Generator FileDraft
genAuthRoutesIndex auth = return $ C.mkTmplFdWithDstAndData tmplFile dstFile (Just tmplData)
  where
    tmplFile = C.srcDirInServerTemplatesDir </> SP.castRel authIndexFileInSrcDir
    dstFile = C.serverSrcDirInServerRootDir </> authIndexFileInSrcDir
    tmplData =
      object
        [ "isExternalAuthEnabled" .= AS.Auth.isExternalAuthEnabled auth
        ]

    authIndexFileInSrcDir :: Path' (Rel C.ServerSrcDir) File'
    authIndexFileInSrcDir = [relfile|routes/auth/index.js|]

genLoginRoute :: AS.Auth.Auth -> Generator FileDraft
genLoginRoute auth = return $ C.mkTmplFdWithDstAndData tmplFile dstFile (Just tmplData)
  where
    loginRouteRelToSrc = [relfile|routes/auth/login.js|]
    tmplFile = C.asTmplFile $ [reldir|src|] </> loginRouteRelToSrc
    dstFile = C.serverSrcDirInServerRootDir </> C.asServerSrcFile loginRouteRelToSrc

    tmplData =
      let userEntityName = AS.refName $ AS.Auth.userEntity auth
       in object
            [ "userEntityUpper" .= (userEntityName :: String),
              "userEntityLower" .= (Util.toLowerFirst userEntityName :: String)
            ]

genSignupRoute :: AS.Auth.Auth -> Generator FileDraft
genSignupRoute auth = return $ C.mkTmplFdWithDstAndData tmplFile dstFile (Just tmplData)
  where
    signupRouteRelToSrc = [relfile|routes/auth/signup.js|]
    tmplFile = C.asTmplFile $ [reldir|src|] </> signupRouteRelToSrc
    dstFile = C.serverSrcDirInServerRootDir </> C.asServerSrcFile signupRouteRelToSrc

    tmplData =
      object
        [ "userEntityLower" .= (Util.toLowerFirst (AS.refName $ AS.Auth.userEntity auth) :: String)
        ]

genMeRoute :: AS.Auth.Auth -> Generator FileDraft
genMeRoute auth = return $ C.mkTmplFdWithDstAndData tmplFile dstFile (Just tmplData)
  where
    meRouteRelToSrc = [relfile|routes/auth/me.js|]
    tmplFile = C.asTmplFile $ [reldir|src|] </> meRouteRelToSrc
    dstFile = C.serverSrcDirInServerRootDir </> C.asServerSrcFile meRouteRelToSrc

    tmplData =
      object
        [ "userEntityLower" .= (Util.toLowerFirst (AS.refName $ AS.Auth.userEntity auth) :: String)
        ]

genPassportAuth :: AS.Auth.Auth -> Generator [FileDraft]
genPassportAuth auth
  | AS.Auth.isExternalAuthEnabled auth =
      sequence
        [ genPassportJs auth,
          copyTmplFile [relfile|routes/auth/passport/generic/provider.js|]
        ]
        <++> genGoogleAuth auth
        <++> genGithubAuth auth
  | otherwise = return []
  where
    copyTmplFile = return . C.mkSrcTmplFd

genPassportJs :: AS.Auth.Auth -> Generator FileDraft
genPassportJs auth = return $ C.mkTmplFdWithDstAndData tmplFile dstFile (Just tmplData)
  where
    tmplFile = C.srcDirInServerTemplatesDir </> SP.castRel passportFileInSrcDir
    dstFile = C.serverSrcDirInServerRootDir </> passportFileInSrcDir
    tmplData =
      object
        [ "providers"
            .= [ buildProviderData "google" "passport-google-oauth20" (AS.Auth.isGoogleAuthEnabled auth),
                 buildProviderData "github" "passport-github2" (AS.Auth.isGithubAuthEnabled auth)
               ]
        ]

    buildProviderData :: String -> String -> Bool -> Aeson.Value
    buildProviderData name npmPackage isEnabled =
      object
        [ "name" .= name,
          "npmPackage" .= npmPackage,
          "isEnabled" .= isEnabled
        ]

    passportFileInSrcDir :: Path' (Rel C.ServerSrcDir) File'
    passportFileInSrcDir = [relfile|routes/auth/passport/passport.js|]

genUtilsJs :: AS.Auth.Auth -> Generator FileDraft
genUtilsJs auth = return $ C.mkTmplFdWithDstAndData tmplFile dstFile (Just tmplData)
  where
    userEntityName = AS.refName $ AS.Auth.userEntity auth
    externalAuthEntityName = maybe "undefined" AS.refName (AS.Auth.externalAuthEntity auth)
    tmplFile = C.srcDirInServerTemplatesDir </> SP.castRel utilsFileInSrcDir
    dstFile = C.serverSrcDirInServerRootDir </> utilsFileInSrcDir
    tmplData =
      object
        [ "userEntityUpper" .= (userEntityName :: String),
          "userEntityLower" .= (Util.toLowerFirst userEntityName :: String),
          "externalAuthEntityLower" .= (Util.toLowerFirst externalAuthEntityName :: String),
          "failureRedirectPath" .= AS.Auth.onAuthFailedRedirectTo auth,
          "successRedirectPath" .= getOnAuthSucceededRedirectToOrDefault auth
        ]

    utilsFileInSrcDir :: Path' (Rel C.ServerSrcDir) File'
    utilsFileInSrcDir = [relfile|routes/auth/utils.js|]

genGoogleAuth :: AS.Auth.Auth -> Generator [FileDraft]
genGoogleAuth auth
  | AS.Auth.isGoogleAuthEnabled auth =
      sequence
        [ return $ C.mkSrcTmplFd [relfile|routes/auth/passport/google/google.js|],
          return $ C.mkSrcTmplFd [relfile|routes/auth/passport/google/defaults.js|],
          return $
            mkAuthConfigFd
              [relfile|routes/auth/passport/generic/config.js|]
              [relfile|routes/auth/passport/google/config.js|]
              (Just configTmplData)
        ]
  | otherwise = return []
  where
    configTmplData = getTmplDataForAuthMethodConfig auth AS.Auth.google

genGithubAuth :: AS.Auth.Auth -> Generator [FileDraft]
genGithubAuth auth
  | AS.Auth.isGithubAuthEnabled auth =
      sequence
        [ return $ C.mkSrcTmplFd [relfile|routes/auth/passport/github/github.js|],
          return $ C.mkSrcTmplFd [relfile|routes/auth/passport/github/defaults.js|],
          return $
            mkAuthConfigFd
              [relfile|routes/auth/passport/generic/config.js|]
              [relfile|routes/auth/passport/github/config.js|]
              (Just configTmplData)
        ]
  | otherwise = return []
  where
    configTmplData = getTmplDataForAuthMethodConfig auth AS.Auth.github

mkAuthConfigFd ::
  Path' (Rel C.ServerTemplatesSrcDir) File' ->
  Path' (Rel C.ServerSrcDir) File' ->
  Maybe Aeson.Value ->
  FileDraft
mkAuthConfigFd pathInTemplatesSrcDir pathInGenProjectSrcDir tmplData =
  C.mkTmplFdWithDstAndData srcPath dstPath tmplData
  where
    srcPath = C.srcDirInServerTemplatesDir </> pathInTemplatesSrcDir
    dstPath = C.serverSrcDirInServerRootDir </> pathInGenProjectSrcDir

getTmplDataForAuthMethodConfig :: AS.Auth.Auth -> (AS.Auth.AuthMethods -> Maybe AS.Auth.SocialLoginConfig) -> Aeson.Value
getTmplDataForAuthMethodConfig auth authMethod =
  object
    [ "doesConfigFnExist" .= isJust maybeConfigFn,
      "configFnImportStatement" .= fromMaybe "" maybeConfigFnImportStmt,
      "configFnIdentifier" .= fromMaybe "" maybeConfigFnImportIdentifier,
      "doesGetUserFieldsFnExist" .= isJust maybeGetUserFieldsFn,
      "getUserFieldsFnImportStatement" .= fromMaybe "" maybeOnSignInFnImportStmt,
      "getUserFieldsFnIdentifier" .= fromMaybe "" maybeOnSignInFnImportIdentifier
    ]
  where
    maybeConfigFn = AS.Auth.configFn =<< authMethod (AS.Auth.methods auth)
    maybeConfigFnImportDetails = getJsImportDetailsForExtFnImport relPosixPathFromAuthMethodDirToExtSrcDir <$> maybeConfigFn
    (maybeConfigFnImportIdentifier, maybeConfigFnImportStmt) = (fst <$> maybeConfigFnImportDetails, snd <$> maybeConfigFnImportDetails)

    maybeGetUserFieldsFn = AS.Auth.getUserFieldsFn =<< authMethod (AS.Auth.methods auth)
    maybeOnSignInFnImportDetails = getJsImportDetailsForExtFnImport relPosixPathFromAuthMethodDirToExtSrcDir <$> maybeGetUserFieldsFn
    (maybeOnSignInFnImportIdentifier, maybeOnSignInFnImportStmt) = (fst <$> maybeOnSignInFnImportDetails, snd <$> maybeOnSignInFnImportDetails)

    relPosixPathFromAuthMethodDirToExtSrcDir :: Path Posix (Rel (Dir C.ServerSrcDir)) (Dir GeneratedExternalCodeDir)
    relPosixPathFromAuthMethodDirToExtSrcDir = [reldirP|../../../../|] </> fromJust (SP.relDirToPosix extServerCodeDirInServerSrcDir)

getOnAuthSucceededRedirectToOrDefault :: AS.Auth.Auth -> String
getOnAuthSucceededRedirectToOrDefault auth = fromMaybe "/" (AS.Auth.onAuthSucceededRedirectTo auth)
