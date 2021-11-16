module Wasp.Generator.WebAppGenerator.AuthG
  ( genAuth,
  )
where

import Data.Aeson (object, (.=))
import StrongPath (reldir, relfile, (</>))
import Wasp.Generator.FileDraft (FileDraft)
import Wasp.Generator.WebAppGenerator.Common as C
import Wasp.Wasp (Wasp, getAuth)
import qualified Wasp.Wasp.Auth as Wasp.Auth

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
genSignup = C.copyTmplAsIs (C.asTmplFile [relfile|src/auth/signup.js|])

-- | Generates file with login function to be used by Wasp developer.
genLogin :: FileDraft
genLogin = C.copyTmplAsIs (C.asTmplFile [relfile|src/auth/login.js|])

-- | Generates file with logout function to be used by Wasp developer.
genLogout :: FileDraft
genLogout = C.copyTmplAsIs (C.asTmplFile [relfile|src/auth/logout.js|])

-- | Generates HOC that handles auth for the given page.
genCreateAuthRequiredPage :: Wasp.Auth.Auth -> FileDraft
genCreateAuthRequiredPage auth =
  C.makeTemplateFD
    (asTmplFile $ [reldir|src|] </> authReqPagePath)
    targetPath
    (Just templateData)
  where
    authReqPagePath = [relfile|auth/pages/createAuthRequiredPage.js|]
    targetPath = C.webAppSrcDirInWebAppRootDir </> asWebAppSrcFile authReqPagePath
    templateData = object ["onAuthFailedRedirectTo" .= Wasp.Auth._onAuthFailedRedirectTo auth]

-- | Generates React hook that Wasp developer can use in a component to get
--   access to the currently logged in user (and check whether user is logged in
--   ot not).
genUseAuth :: FileDraft
genUseAuth = C.copyTmplAsIs (C.asTmplFile [relfile|src/auth/useAuth.js|])

genAuthForms :: [FileDraft]
genAuthForms =
  [ genLoginForm,
    genSignupForm
  ]

genLoginForm :: FileDraft
genLoginForm = C.copyTmplAsIs (C.asTmplFile [relfile|src/auth/forms/Login.js|])

genSignupForm :: FileDraft
genSignupForm = C.copyTmplAsIs (C.asTmplFile [relfile|src/auth/forms/Signup.js|])
