module Generator.ServerGenerator.AuthG
    ( genAuth
    ) where

import qualified Path as P
import           Data.Aeson (object, (.=))

import qualified Util
import Wasp (Wasp, getAuth)
import qualified Wasp.Auth
import Generator.FileDraft (FileDraft)
import qualified Generator.ServerGenerator.Common as C
import StrongPath ((</>))

genAuth :: Wasp -> [FileDraft]
genAuth wasp = case maybeAuth of
    Just auth -> [ genCoreAuth auth
                   -- Auth routes
                 , genAuthRoutesIndex
                 , genLoginRoute auth
                 , genSignupRoute
                 , genMeRoute auth
                 ]
    Nothing   -> []
    where
        maybeAuth = getAuth wasp

-- | Generates core/auth file which contains auth middleware and createUser() function.
genCoreAuth :: Wasp.Auth.Auth -> FileDraft
genCoreAuth auth = C.makeTemplateFD tmplFile dstFile (Just tmplData)
    where
        coreAuthRelToSrc = [P.relfile|core/auth.js|]
        tmplFile = C.asTmplFile $ [P.reldir|src|] P.</> coreAuthRelToSrc
        dstFile = C.serverSrcDirInServerRootDir </> (C.asServerSrcFile coreAuthRelToSrc)

        tmplData = let userEntity = (Wasp.Auth._userEntity auth) in object
            [ "userEntityUpper" .= userEntity
            , "userEntityLower" .= Util.toLowerFirst userEntity
            ]

genAuthRoutesIndex :: FileDraft
genAuthRoutesIndex = C.copySrcTmplAsIs (C.asTmplSrcFile [P.relfile|routes/auth/index.js|])

genLoginRoute :: Wasp.Auth.Auth -> FileDraft
genLoginRoute auth = C.makeTemplateFD tmplFile dstFile (Just tmplData)
    where
        loginRouteRelToSrc = [P.relfile|routes/auth/login.js|]
        tmplFile = C.asTmplFile $ [P.reldir|src|] P.</> loginRouteRelToSrc
        dstFile = C.serverSrcDirInServerRootDir </> (C.asServerSrcFile loginRouteRelToSrc)

        tmplData = let userEntity = (Wasp.Auth._userEntity auth) in object
            [ "userEntityUpper" .= userEntity
            , "userEntityLower" .= Util.toLowerFirst userEntity
            ]

genSignupRoute :: FileDraft
genSignupRoute = C.copySrcTmplAsIs (C.asTmplSrcFile [P.relfile|routes/auth/signup.js|])

genMeRoute :: Wasp.Auth.Auth -> FileDraft
genMeRoute auth = C.makeTemplateFD tmplFile dstFile (Just tmplData)
    where
        meRouteRelToSrc = [P.relfile|routes/auth/me.js|]
        tmplFile = C.asTmplFile $ [P.reldir|src|] P.</> meRouteRelToSrc
        dstFile = C.serverSrcDirInServerRootDir </> (C.asServerSrcFile meRouteRelToSrc)

        tmplData = object
            [ "userEntityLower" .= Util.toLowerFirst (Wasp.Auth._userEntity auth)
            ]
