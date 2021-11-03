module Wasp.Generator.WebAppGenerator.AuthG
  ( genAuth,
  )
where

import Data.Aeson (object, (.=))
import Data.Aeson.Types (Pair)
import StrongPath (File', Path', Rel', reldir, relfile, (</>))
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
      ++ genAuthForms auth
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
  compileTmplToSamePath
    [relfile|auth/pages/createAuthRequiredPage.js|]
    ["onAuthFailedRedirectTo" .= Wasp.Auth._onAuthFailedRedirectTo auth]

-- | Generates React hook that Wasp developer can use in a component to get
--   access to the currently logged in user (and check whether user is logged in
--   ot not).
genUseAuth :: FileDraft
genUseAuth = C.copyTmplAsIs (C.asTmplFile [relfile|src/auth/useAuth.js|])

genAuthForms :: Wasp.Auth.Auth -> [FileDraft]
genAuthForms auth =
  [ genLoginForm auth,
    genSignupForm auth
  ]

genLoginForm :: Wasp.Auth.Auth -> FileDraft
genLoginForm auth =
  compileTmplToSamePath
    [relfile|auth/forms/Login.js|]
    ["onAuthSucceededRedirectTo" .= Wasp.Auth._onAuthSucceededRedirectTo auth]

genSignupForm :: Wasp.Auth.Auth -> FileDraft
genSignupForm auth =
  compileTmplToSamePath
    [relfile|auth/forms/Signup.js|]
    ["onAuthSucceededRedirectTo" .= Wasp.Auth._onAuthSucceededRedirectTo auth]

compileTmplToSamePath :: Path' Rel' File' -> [Pair] -> FileDraft
compileTmplToSamePath tmplFileInTmplSrcDir keyValuePairs =
  C.makeTemplateFD
    (asTmplFile $ [reldir|src|] </> tmplFileInTmplSrcDir)
    targetPath
    (Just templateData)
  where
    targetPath = C.webAppSrcDirInWebAppRootDir </> asWebAppSrcFile tmplFileInTmplSrcDir
    templateData = object keyValuePairs
