module Generator.WebAppGenerator.AuthG
  ( genAuth,
  )
where

import Data.Aeson (object, (.=))
import Generator.FileDraft (FileDraft)
import Generator.WebAppGenerator.Common as C
import qualified Path as P
import StrongPath ((</>))
import Wasp (Wasp, getAuth)
import qualified Wasp.Auth

genAuth :: Wasp -> [FileDraft]
genAuth wasp = case maybeAuth of
  Just auth ->
    [ genSignup,
      genLogin,
      genLogout,
      genUseAuth,
      genCreateAuthRequiredPage auth
    ]
      ++ genAuthForms
  Nothing -> []
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

-- | Generates HOC that handles auth for the given page.
genCreateAuthRequiredPage :: Wasp.Auth.Auth -> FileDraft
genCreateAuthRequiredPage auth =
  C.makeTemplateFD
    (asTmplFile $ [P.reldir|src|] P.</> authReqPagePath)
    targetPath
    (Just templateData)
  where
    authReqPagePath = [P.relfile|auth/pages/createAuthRequiredPage.js|]
    targetPath = C.webAppSrcDirInWebAppRootDir </> (asWebAppSrcFile authReqPagePath)
    templateData = object ["onAuthFailedRedirectTo" .= (Wasp.Auth._onAuthFailedRedirectTo auth)]

-- | Generates React hook that Wasp developer can use in a component to get
--   access to the currently logged in user (and check whether user is logged in
--   ot not).
genUseAuth :: FileDraft
genUseAuth = C.copyTmplAsIs (C.asTmplFile [P.relfile|src/auth/useAuth.js|])

genAuthForms :: [FileDraft]
genAuthForms =
  [ genLoginForm,
    genSignupForm
  ]

genLoginForm :: FileDraft
genLoginForm = C.copyTmplAsIs (C.asTmplFile [P.relfile|src/auth/forms/Login.js|])

genSignupForm :: FileDraft
genSignupForm = C.copyTmplAsIs (C.asTmplFile [P.relfile|src/auth/forms/Signup.js|])
