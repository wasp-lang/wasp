module Wasp.Generator.SdkGenerator.Auth.OAuthAuthG
  ( genOAuthAuth,
  )
where

import Data.Aeson (object, (.=))
import StrongPath (reldir, relfile, (</>))
import qualified Wasp.AppSpec.App.Auth as AS.Auth
import Wasp.Generator.Auth.Provider
  ( OAuthProviderSpec (..),
    enabledOAuthProviders,
    isOAuthEnabled,
    serverOAuthLoginUrl,
  )
import Wasp.Generator.FileDraft (FileDraft)
import Wasp.Generator.Monad (Generator)
import Wasp.Generator.SdkGenerator.Common as C

genOAuthAuth :: AS.Auth.Auth -> Generator [FileDraft]
genOAuthAuth auth
  | isOAuthEnabled auth = return $ map makeHelperFd (enabledOAuthProviders auth)
  | otherwise = return []
  where
    makeHelperFd (spec, _config) =
      mkTmplFdWithDstAndData
        [relfile|auth/helpers/_Provider.tsx|]
        ([reldir|auth/helpers|] </> helpersFp)
        (Just tmplData)
      where
        helpersFp = sdkHelperFile spec
        tmplData =
          object
            [ "signInPath" .= serverOAuthLoginUrl spec,
              "displayName" .= displayName spec
            ]

    sdkHelperFile spec = case slug spec of
      "google" -> [relfile|Google.tsx|]
      "github" -> [relfile|GitHub.tsx|]
      "discord" -> [relfile|Discord.tsx|]
      "keycloak" -> [relfile|Keycloak.tsx|]
      "slack" -> [relfile|Slack.tsx|]
      "microsoft" -> [relfile|Microsoft.tsx|]
      other -> error $ "Unknown OAuth provider: " ++ other
