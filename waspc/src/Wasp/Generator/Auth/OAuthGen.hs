module Wasp.Generator.Auth.OAuthGen
  ( OAuthGenContext (..),
    genForEachEnabledOAuth,
  )
where

import qualified Wasp.AppSpec.App.Auth as AS.Auth
import Wasp.Generator.Auth.Provider (OAuthProviderSpec, enabledOAuthProviders)
import Wasp.Generator.FileDraft (FileDraft)
import Wasp.Generator.Monad (Generator)

data OAuthGenContext = OAuthGenContext
  { oAuthSpec :: OAuthProviderSpec,
    oAuthUserConfig :: AS.Auth.ExternalAuthConfig
  }

genForEachEnabledOAuth ::
  AS.Auth.Auth ->
  (OAuthGenContext -> Generator [FileDraft]) ->
  Generator [FileDraft]
genForEachEnabledOAuth auth genProvider =
  concat <$> mapM genOne (enabledOAuthProviders auth)
  where
    genOne (spec, cfg) =
      genProvider
        OAuthGenContext
          { oAuthSpec = spec,
            oAuthUserConfig = cfg
          }
