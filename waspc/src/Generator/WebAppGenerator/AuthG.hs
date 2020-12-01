module Generator.WebAppGenerator.AuthG
    ( genAuth
    ) where

import qualified Path as P

import Wasp (Wasp, getAuth)
import Generator.FileDraft (FileDraft)
import Generator.WebAppGenerator.Common as C

genAuth :: Wasp -> [FileDraft]
genAuth wasp = case maybeAuth of
    Just _    -> [ genSignup
                 , genLogin
                 , genLogout
                 , genUseAuth
                 ] ++ genAuthPages
    Nothing   -> []
    where
        maybeAuth = getAuth wasp

-- | Generates file with signup function to be used by Wasp developer.
genSignup :: FileDraft
genSignup = C.copyTmplAsIs (C.asTmplFile [P.relfile|src/auth/signup.js|])

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

genAuthPages :: [FileDraft]
genAuthPages =
    [ genSignupPage
    , genLoginPage
    ]

genLoginPage :: FileDraft
genLoginPage = C.copyTmplAsIs (C.asTmplFile [P.relfile|src/auth/pages/Login.js|])

genSignupPage :: FileDraft
genSignupPage = C.copyTmplAsIs (C.asTmplFile [P.relfile|src/auth/pages/Signup.js|])
