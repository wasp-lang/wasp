module Wasp.Generator.ServerGenerator.AuthG
  ( genAuth,
  )
where

import Data.Aeson (object, (.=))
import StrongPath (reldir, relfile, (</>))
import Wasp.AppSpec (AppSpec)
import qualified Wasp.AppSpec as AS
import qualified Wasp.AppSpec.App as AS.App
import qualified Wasp.AppSpec.App.Auth as AS.Auth
import Wasp.AppSpec.Valid (Valid, ($^), (<$^>), (<$^^>))
import qualified Wasp.AppSpec.Valid.AppSpec as VAS
import Wasp.Generator.FileDraft (FileDraft)
import Wasp.Generator.Monad (Generator)
import qualified Wasp.Generator.ServerGenerator.Common as C
import qualified Wasp.Util as Util

genAuth :: Valid AppSpec -> Generator [FileDraft]
genAuth spec = case maybeAuth of
  Just auth ->
    sequence
      [ genCoreAuth auth,
        genAuthMiddleware auth,
        -- Auth routes
        genAuthRoutesIndex,
        genLoginRoute auth,
        genSignupRoute auth,
        genMeRoute auth
      ]
  Nothing -> return []
  where
    maybeAuth = AS.App.auth . snd <$^> VAS.getApp spec

-- | Generates core/auth file which contains auth middleware and createUser() function.
genCoreAuth :: Valid AS.Auth.Auth -> Generator FileDraft
genCoreAuth auth = return $ C.mkTmplFdWithDstAndData tmplFile dstFile (Just tmplData)
  where
    coreAuthRelToSrc = [relfile|core/auth.js|]
    tmplFile = C.asTmplFile $ [reldir|src|] </> coreAuthRelToSrc
    dstFile = C.serverSrcDirInServerRootDir </> C.asServerSrcFile coreAuthRelToSrc

    tmplData =
      let userEntityName = AS.refName $ AS.Auth.userEntity $^ auth
       in object
            [ "userEntityUpper" .= (userEntityName :: String),
              "userEntityLower" .= (Util.toLowerFirst userEntityName :: String)
            ]

genAuthMiddleware :: Valid AS.Auth.Auth -> Generator FileDraft
genAuthMiddleware auth = return $ C.mkTmplFdWithDstAndData tmplFile dstFile (Just tmplData)
  where
    authMiddlewareRelToSrc = [relfile|core/auth/prismaMiddleware.js|]
    tmplFile = C.asTmplFile $ [reldir|src|] </> authMiddlewareRelToSrc
    dstFile = C.serverSrcDirInServerRootDir </> C.asServerSrcFile authMiddlewareRelToSrc

    tmplData =
      let userEntityName = AS.refName $ AS.Auth.userEntity $^ auth
       in object
            [ "userEntityUpper" .= (userEntityName :: String)
            ]

genAuthRoutesIndex :: Generator FileDraft
genAuthRoutesIndex = return $ C.mkSrcTmplFd (C.asTmplSrcFile [relfile|routes/auth/index.js|])

genLoginRoute :: Valid AS.Auth.Auth -> Generator FileDraft
genLoginRoute auth = return $ C.mkTmplFdWithDstAndData tmplFile dstFile (Just tmplData)
  where
    loginRouteRelToSrc = [relfile|routes/auth/login.js|]
    tmplFile = C.asTmplFile $ [reldir|src|] </> loginRouteRelToSrc
    dstFile = C.serverSrcDirInServerRootDir </> C.asServerSrcFile loginRouteRelToSrc

    tmplData =
      let userEntityName = AS.refName $ AS.Auth.userEntity $^ auth
       in object
            [ "userEntityUpper" .= (userEntityName :: String),
              "userEntityLower" .= (Util.toLowerFirst userEntityName :: String)
            ]

genSignupRoute :: Valid AS.Auth.Auth -> Generator FileDraft
genSignupRoute auth = return $ C.mkTmplFdWithDstAndData tmplFile dstFile (Just tmplData)
  where
    signupRouteRelToSrc = [relfile|routes/auth/signup.js|]
    tmplFile = C.asTmplFile $ [reldir|src|] </> signupRouteRelToSrc
    dstFile = C.serverSrcDirInServerRootDir </> C.asServerSrcFile signupRouteRelToSrc

    tmplData =
      object
        [ "userEntityLower" .= (Util.toLowerFirst (AS.refName $ AS.Auth.userEntity $^ auth) :: String)
        ]

genMeRoute :: Valid AS.Auth.Auth -> Generator FileDraft
genMeRoute auth = return $ C.mkTmplFdWithDstAndData tmplFile dstFile (Just tmplData)
  where
    meRouteRelToSrc = [relfile|routes/auth/me.js|]
    tmplFile = C.asTmplFile $ [reldir|src|] </> meRouteRelToSrc
    dstFile = C.serverSrcDirInServerRootDir </> C.asServerSrcFile meRouteRelToSrc

    tmplData =
      object
        [ "userEntityLower" .= (Util.toLowerFirst (AS.refName $ AS.Auth.userEntity $^ auth) :: String)
        ]
