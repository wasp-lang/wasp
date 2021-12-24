module Wasp.Generator.WebAppGenerator.AuthG
  ( genAuth,
  )
where

import Data.Aeson (object, (.=))
import Data.Aeson.Types (Pair)
import Data.Maybe (fromMaybe)
import StrongPath (File', Path', Rel', reldir, relfile, (</>))
import Wasp.AppSpec (AppSpec)
import qualified Wasp.AppSpec as AS
import qualified Wasp.AppSpec.App as AS.App
import qualified Wasp.AppSpec.App.Auth as AS.Auth
import Wasp.Generator.FileDraft (FileDraft)
import Wasp.Generator.WebAppGenerator.Common as C

genAuth :: AppSpec -> [FileDraft]
genAuth spec = case maybeAuth of
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
    maybeAuth = AS.App.auth $ snd $ AS.getApp spec

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
genCreateAuthRequiredPage :: AS.Auth.Auth -> FileDraft
genCreateAuthRequiredPage auth =
  compileTmplToSamePath
    [relfile|auth/pages/createAuthRequiredPage.js|]
    ["onAuthFailedRedirectTo" .= AS.Auth.onAuthFailedRedirectTo auth]

-- | Generates React hook that Wasp developer can use in a component to get
--   access to the currently logged in user (and check whether user is logged in
--   ot not).
genUseAuth :: FileDraft
genUseAuth = C.copyTmplAsIs (C.asTmplFile [relfile|src/auth/useAuth.js|])

genAuthForms :: AS.Auth.Auth -> [FileDraft]
genAuthForms auth =
  [ genLoginForm auth,
    genSignupForm auth
  ]

genLoginForm :: AS.Auth.Auth -> FileDraft
genLoginForm auth =
  -- TODO: Logic that says "/" is a default redirect on success is duplicated here and in the function below.
  --   We should remove that duplication.
  compileTmplToSamePath
    [relfile|auth/forms/Login.js|]
    ["onAuthSucceededRedirectTo" .= fromMaybe "/" (AS.Auth.onAuthSucceededRedirectTo auth)]

genSignupForm :: AS.Auth.Auth -> FileDraft
genSignupForm auth =
  compileTmplToSamePath
    [relfile|auth/forms/Signup.js|]
    ["onAuthSucceededRedirectTo" .= fromMaybe "/" (AS.Auth.onAuthSucceededRedirectTo auth)]

compileTmplToSamePath :: Path' Rel' File' -> [Pair] -> FileDraft
compileTmplToSamePath tmplFileInTmplSrcDir keyValuePairs =
  C.makeTemplateFD
    (asTmplFile $ [reldir|src|] </> tmplFileInTmplSrcDir)
    targetPath
    (Just templateData)
  where
    targetPath = C.webAppSrcDirInWebAppRootDir </> asWebAppSrcFile tmplFileInTmplSrcDir
    templateData = object keyValuePairs
