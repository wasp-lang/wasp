export { TimeSpan, createJWTHelpers } from "./jwt";

export { hashPassword, verifyPassword } from "./password";

export { parseCookies } from "oslo/cookie";

export {
  generateAndStoreOAuthState,
  getOAuthCookieName,
  getOAuthCookieOptions,
  getOAuthCookieValueFromHeader,
  validateAndGetOAuthState,
  type OAuthCookieOptions,
  type OAuthStateFieldName,
  type OAuthStateFor,
  type OAuthStateWithCodeFor,
  type OAuthType,
} from "./oauthState";

export {
  createProviderId,
  getProviderData,
  getProviderDataWithPassword,
  mergeAndSerializeProviderDataUpdates,
  normalizeProviderUserId,
  sanitizeAndSerializeProviderData,
  type EmailProviderData,
  type OAuthProviderData,
  type PossibleProviderData,
  type ProviderId,
  type ProviderName,
  type UsernameProviderData,
} from "./providerData";
