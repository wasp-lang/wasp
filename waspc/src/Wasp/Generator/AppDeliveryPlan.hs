module Wasp.Generator.AppDeliveryPlan
  ( AppDeliveryPlan (..),
    OAuthLoginCompletion (..),
    makeAppDeliveryPlan,
  )
where

import Wasp.AppSpec (AppDeliveryMode (..), AppSpec, appDeliveryMode)
import qualified Wasp.AppSpec as AS
import Wasp.AppSpec.Valid (isAuthEnabled)

data AppDeliveryPlan = AppDeliveryPlan
  { deliveryMode :: AppDeliveryMode,
    waspApiMountPath :: String,
    authEnabled :: Bool,
    serveClientAssets :: Bool,
    oauthLoginCompletion :: OAuthLoginCompletion
  }
  deriving (Show, Eq)

data OAuthLoginCompletion
  = EstablishSessionInProviderCallback
  | ExchangeSessionHandoffCode
  deriving (Show, Eq)

makeAppDeliveryPlan :: AppSpec -> AppDeliveryPlan
makeAppDeliveryPlan spec =
  AppDeliveryPlan
    { deliveryMode = appDeliveryMode spec,
      waspApiMountPath = "/api",
      authEnabled = isAuthEnabled spec,
      serveClientAssets = appDeliveryMode spec == Integrated && not (AS.isDevelopment spec),
      oauthLoginCompletion = case appDeliveryMode spec of
        Integrated -> EstablishSessionInProviderCallback
        Split -> ExchangeSessionHandoffCode
    }
