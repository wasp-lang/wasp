module Wasp.Generator.SdkGenerator.Client.AuthPagesGenerator
  ( genClientAuthPages,
  )
where

import Data.Aeson (object, (.=))
import StrongPath (relfile)
import Wasp.AppSpec (AppSpec)
import qualified Wasp.AppSpec.App as AS.App
import Wasp.AppSpec.Valid (getApp)
import Wasp.Generator.FileDraft (FileDraft)
import Wasp.Generator.Monad (Generator)
import qualified Wasp.Generator.SdkGenerator.Auth.Common as Auth
import qualified Wasp.Generator.SdkGenerator.Common as C

genClientAuthPages :: AppSpec -> Generator [FileDraft]
genClientAuthPages spec =
  sequence
    [ return $ C.mkTmplFdWithData [relfile|client/auth/pages/OAuthCallback.tsx|] tmplData,
      return $ C.mkTmplFdWithData [relfile|client/auth/pages/createAuthRequiredPage.jsx|] tmplData
    ]
  where
    tmplData =
      object
        [ "onAuthSucceededRedirectTo" .= maybe "/" Auth.getOnAuthSucceededRedirectToOrDefault maybeAuth,
          "onAuthFailedRedirectTo" .= maybe "/login" Auth.getOnAuthFailedRedirectTo maybeAuth
        ]
    maybeAuth = AS.App.auth $ snd $ getApp spec
