module Wasp.Generator.WebAppGenerator.Auth.EmailAuthG
  ( genEmailAuth,
  )
where

import Data.Aeson (object, (.=))
import StrongPath (relfile)
import qualified Wasp.AppSpec.App.Auth as AS.Auth
import Wasp.Generator.FileDraft (FileDraft)
import Wasp.Generator.Monad (Generator)
import Wasp.Generator.WebAppGenerator.Auth.Common (getOnAuthSucceededRedirectToOrDefault)
import Wasp.Generator.WebAppGenerator.Common as C
import Wasp.Util ((<++>))

genEmailAuth :: AS.Auth.Auth -> Generator [FileDraft]
genEmailAuth auth =
  sequence
    [ genIndex
    ]
    <++> genActions auth
    <++> genComponents auth

genIndex :: Generator FileDraft
genIndex = return $ C.mkSrcTmplFd [relfile|auth/email/index.ts|]

genActions :: AS.Auth.Auth -> Generator [FileDraft]
genActions auth
  | AS.Auth.isEmailAuthEnabled auth =
    sequence
      [ copyTmplFile [relfile|auth/email/actions/login.ts|],
        copyTmplFile [relfile|auth/email/actions/signup.ts|],
        copyTmplFile [relfile|auth/email/actions/passwordReset.ts|]
      ]
  | otherwise = return []
  where
    copyTmplFile = return . C.mkSrcTmplFd

genComponents :: AS.Auth.Auth -> Generator [FileDraft]
genComponents auth
  | AS.Auth.isUsernameAndPasswordAuthEnabled auth =
    sequence
      [ genLoginComponent auth,
        genSignupComponent auth
      ]
  | otherwise = return []

genLoginComponent :: AS.Auth.Auth -> Generator FileDraft
genLoginComponent auth =
  return $
    C.mkTmplFdWithData
      [relfile|src/auth/email/components/Login.jsx|]
      (object ["onAuthSucceededRedirectTo" .= getOnAuthSucceededRedirectToOrDefault auth])

genSignupComponent :: AS.Auth.Auth -> Generator FileDraft
genSignupComponent auth =
  return $
    C.mkTmplFdWithData
      [relfile|src/auth/email/components/Signup.jsx|]
      (object ["onAuthSucceededRedirectTo" .= getOnAuthSucceededRedirectToOrDefault auth])
