module Wasp.Generator.AuthProviders.Common
  ( ProviderId,
    makeProviderId,
    fromProviderId,
  )
where

-- Unique identifier of the auth provider, it's used in the
-- URL of the login page, e.g. /auth/{providerId}/login, so it should
-- include only characters that are allowed in URLs
data ProviderId = ProviderId String
  deriving (Show, Eq)

-- Create ProviderId from String, but only if it's valid
-- (i.e. it contains only a-z and dashes and it's not empty).
makeProviderId :: String -> Maybe ProviderId
makeProviderId s
  | s == "" = Nothing
  | otherwise = if all isValidChar s then Just (ProviderId s) else Nothing
  where
    isValidChar c = c `elem` ['a' .. 'z'] || c == '-'

fromProviderId :: ProviderId -> String
fromProviderId (ProviderId s) = s
