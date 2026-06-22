export { TimeSpan, createJWTHelpers } from "./jwt";

export { hashPassword, verifyPassword } from "./password";

export { parseCookies } from "oslo/cookie";

export {
  AuthServiceError,
  type AuthCoreAdapters,
  type AuthHooks,
  type AuthId,
  type AuthIdentity,
  type AuthRepository,
  type AuthServiceAdapters,
  type AuthServiceErrorCode,
  type AuthWithUser,
  type Clock,
  type CreatedUserWithAuth,
  type EmailAuthAdapters,
  type EmailTokenService,
  type EmailVerificationService,
  type MaybePromise,
  type OAuthAuthAdapters,
  type OAuthRedirects,
  type OneTimeCodeStore,
  type PasswordResetService,
  type ProviderIdFor,
  type RandomSource,
  type Session,
  type SessionService,
  type WorkSimulator,
} from "./authServiceAdapters";

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

export {
  loginWithEmail,
  signupWithEmail,
  type EmailAuthLoginResult,
  type EmailLoginAdapters,
  type EmailLoginArgs,
  type EmailSignupAdapters,
  type EmailSignupArgs,
  type EmailSignupResult,
} from "./emailAuth";

export {
  loginWithUsername,
  signupWithUsername,
  type UsernameAuthLoginResult,
  type UsernameLoginAdapters,
  type UsernameLoginArgs,
  type UsernameSignupAdapters,
  type UsernameSignupArgs,
  type UsernameSignupResult,
} from "./usernameAuth";
