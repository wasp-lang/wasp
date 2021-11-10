module Wasp.Generator.ServerGenerator.AuthG
  ( genAuth,
  )
where

import Data.Aeson (object, (.=))
import StrongPath (reldir, relfile, (</>))
import Wasp.Generator.FileDraft (FileDraft)
import qualified Wasp.Generator.ServerGenerator.Common as C
import qualified Wasp.Util as Util
import Wasp.Wasp (Wasp, getAuth)
import qualified Wasp.Wasp.Auth as Wasp.Auth

genAuth :: Wasp -> [FileDraft]
genAuth wasp = case maybeAuth of
  Just auth ->
    [ genCoreAuth auth,
      -- Auth routes
      genAuthRoutesIndex,
      genLoginRoute auth,
      genSignupRoute auth,
      genMeRoute auth
    ]
  Nothing -> []
  where
    maybeAuth = getAuth wasp

-- | Generates core/auth file which contains auth middleware and createUser() function.
genCoreAuth :: Wasp.Auth.Auth -> FileDraft
genCoreAuth auth = C.makeTemplateFD tmplFile dstFile (Just tmplData)
  where
    coreAuthRelToSrc = [relfile|core/auth.js|]
    tmplFile = C.asTmplFile $ [reldir|src|] </> coreAuthRelToSrc
    dstFile = C.serverSrcDirInServerRootDir </> C.asServerSrcFile coreAuthRelToSrc

    tmplData =
      let userEntity = Wasp.Auth._userEntity auth
       in object
            [ "userEntityUpper" .= userEntity,
              "userEntityLower" .= Util.toLowerFirst userEntity
            ]

genAuthRoutesIndex :: FileDraft
genAuthRoutesIndex = C.copySrcTmplAsIs (C.asTmplSrcFile [relfile|routes/auth/index.js|])

genLoginRoute :: Wasp.Auth.Auth -> FileDraft
genLoginRoute auth = C.makeTemplateFD tmplFile dstFile (Just tmplData)
  where
    loginRouteRelToSrc = [relfile|routes/auth/login.js|]
    tmplFile = C.asTmplFile $ [reldir|src|] </> loginRouteRelToSrc
    dstFile = C.serverSrcDirInServerRootDir </> C.asServerSrcFile loginRouteRelToSrc

    tmplData =
      let userEntity = Wasp.Auth._userEntity auth
       in object
            [ "userEntityUpper" .= userEntity,
              "userEntityLower" .= Util.toLowerFirst userEntity
            ]

genSignupRoute :: Wasp.Auth.Auth -> FileDraft
genSignupRoute auth = C.makeTemplateFD tmplFile dstFile (Just tmplData)
  where
    signupRouteRelToSrc = [relfile|routes/auth/signup.js|]
    tmplFile = C.asTmplFile $ [reldir|src|] </> signupRouteRelToSrc
    dstFile = C.serverSrcDirInServerRootDir </> C.asServerSrcFile signupRouteRelToSrc

    tmplData =
      object
        [ "userEntityLower" .= Util.toLowerFirst (Wasp.Auth._userEntity auth)
        ]

genMeRoute :: Wasp.Auth.Auth -> FileDraft
genMeRoute auth = C.makeTemplateFD tmplFile dstFile (Just tmplData)
  where
    meRouteRelToSrc = [relfile|routes/auth/me.js|]
    tmplFile = C.asTmplFile $ [reldir|src|] </> meRouteRelToSrc
    dstFile = C.serverSrcDirInServerRootDir </> C.asServerSrcFile meRouteRelToSrc

    tmplData =
      object
        [ "userEntityLower" .= Util.toLowerFirst (Wasp.Auth._userEntity auth)
        ]
