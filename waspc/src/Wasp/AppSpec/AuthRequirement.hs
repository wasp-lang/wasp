{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Wasp.AppSpec.AuthRequirement
  ( AuthRequirement (..),
    isAuthRequired,
    requiredAuthProviderIds,
    isAuthRequiredWithDefault,
  )
where

import Data.Aeson (FromJSON (..), ToJSON (..))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as AesonTypes
import Data.Data (Data)
import GHC.Generics (Generic)

-- | Whether (and through which providers) a page or operation requires
-- authentication. On the wire this is a boolean or a list of auth provider
-- ids: @false@ = no auth, @true@ = any valid session, @["wasp", ...]@ = only
-- sessions minted by one of the listed providers.
--
-- The restriction compares against the provider recorded on the session when
-- it was minted -- a pure data check, never a call into provider code.
data AuthRequirement
  = AuthNotRequired
  | AuthRequiredForAnyProvider
  | AuthRequiredForProviders [String]
  deriving (Show, Eq, Data, Generic)

instance FromJSON AuthRequirement where
  parseJSON (Aeson.Bool False) = pure AuthNotRequired
  parseJSON (Aeson.Bool True) = pure AuthRequiredForAnyProvider
  parseJSON value@(Aeson.Array _) = AuthRequiredForProviders <$> parseJSON value
  parseJSON value = AesonTypes.typeMismatch "Bool or [String] (auth provider ids)" value

instance ToJSON AuthRequirement where
  toJSON AuthNotRequired = Aeson.Bool False
  toJSON AuthRequiredForAnyProvider = Aeson.Bool True
  toJSON (AuthRequiredForProviders providerIds) = toJSON providerIds

isAuthRequired :: AuthRequirement -> Bool
isAuthRequired AuthNotRequired = False
isAuthRequired AuthRequiredForAnyProvider = True
isAuthRequired (AuthRequiredForProviders _) = True

-- | The provider ids the requirement restricts to, or Nothing when any
-- provider (or none) is acceptable.
requiredAuthProviderIds :: AuthRequirement -> Maybe [String]
requiredAuthProviderIds (AuthRequiredForProviders providerIds) = Just providerIds
requiredAuthProviderIds _ = Nothing

-- | Resolves an optional requirement to "is auth required", falling back to
-- the given default when the field was omitted (pages default to False,
-- operations default to whether the app has auth at all).
isAuthRequiredWithDefault :: Bool -> Maybe AuthRequirement -> Bool
isAuthRequiredWithDefault = flip maybe isAuthRequired
