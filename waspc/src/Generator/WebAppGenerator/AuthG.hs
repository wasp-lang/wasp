module Generator.WebAppGenerator.AuthG
    ( genAuth
    ) where

import qualified Path as P

import Wasp (Wasp, getAuth)
import Generator.FileDraft (FileDraft)
import Generator.WebAppGenerator.Common as C

genAuth :: Wasp -> [FileDraft]
genAuth wasp = case maybeAuth of
    Just _    -> [ genApi
                 , genLogin
                 , genLogout
                 , genUseAuth
                 ]
    Nothing   -> []
    where
        maybeAuth = getAuth wasp

-- | Generates api.js file which contains token management and configured api (e.g. axios) instance.
genApi :: FileDraft
genApi = C.copyTmplAsIs (C.asTmplFile [P.relfile|src/api.js|])

-- | Generates file with login function to be used by Wasp developer.
genLogin :: FileDraft
genLogin = C.copyTmplAsIs (C.asTmplFile [P.relfile|src/auth/login.js|])

-- | Generates file with logout function to be used by Wasp developer.
genLogout :: FileDraft
genLogout = C.copyTmplAsIs (C.asTmplFile [P.relfile|src/auth/logout.js|])

-- | Generates React hook that Wasp developer can use in a component to get
--   access to the currently logged in user (and check whether user is logged in
--   ot not).
genUseAuth :: FileDraft
genUseAuth = C.copyTmplAsIs (C.asTmplFile [P.relfile|src/auth/useAuth.js|])


{-
-- | Generates React hook that Wasp developer can use in a component to get
--   access to the currently logged in user (and check whether user is logged in
--   ot not).
genUseUser :: Wasp.Auth.Auth -> FileDraft
genUseUser auth = C.makeTemplateFD tmplFile dstFile (Just tmplData)
    where
        tmplFile = C.asTmplFile [P.relfile|src/auth/_useUser.js|]
        dstFile = C.asWebAppFile $ [P.reldir|src/auth/|] P.</> fromJust (getUseUserDstFileName auth)
        tmplData = object
            [ "userEntityLower" .= Util.toLowerFirst (Wasp.Auth._userEntity auth)
            , "userEntity" .= (Wasp.Auth._userEntity auth)
            ]

        getUseUserDstFileName :: Wasp.Auth.Auth -> Maybe (P.Path P.Rel P.File)
        getUseUserDstFileName a = P.parseRelFile ("use" ++ (Wasp.Auth._userEntity a) ++ ".js")
-}
