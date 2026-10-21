export { doFakeWork } from "./fakeWork.js";
export {
  createAuthHooks,
  type AuthHookTypes,
  type AuthHooks,
  type AuthHooksConfig,
  type InternalAuthHookParams,
  type OAuthFlowData,
  type OnAfterEmailVerifiedHook,
  type OnAfterLoginHook,
  type OnAfterSignupHook,
  type OnBeforeLoginHook,
  type OnBeforeOAuthRedirectHook,
  type OnBeforeSignupHook,
  type UserDefinedAuthHooks,
} from "./hooks.js";
export {
  createAuthIdentities,
  type AuthIdentities,
  type AuthIdentitiesConfig,
  type AuthWithUser,
  type CreateUserResult,
  type FindAuthWithUserResult,
  type PossibleUserFields,
  type PrismaModelDelegate,
  type UserSignupFields,
} from "./identities.js";
export { TimeSpan, createJWTHelpers, type JwtHelpers } from "./jwt.js";
export { createAuthMiddleware, type RequestWithAuth } from "./middleware.js";
export { hashPassword, verifyPassword } from "./password.js";
export {
  ensurePasswordIsHashed,
  sanitizeAndSerializeProviderData,
} from "./providerDataStorage.js";
export { type ProviderConfig, type RequestWithWasp } from "./providers.js";
export {
  createEmailAuthProvider,
  type EmailAuthConfig,
  type EmailAuthDependencies,
} from "./providers/email/provider.js";
export {
  type GetPasswordResetEmailContentFn,
  type GetVerificationEmailContentFn,
} from "./providers/email/types.js";
export {
  createEmailVerification,
  type AuthEmail,
  type EmailVerification,
  type EmailVerificationConfig,
} from "./providers/email/verification.js";
export { type UserDefinedConfigFn } from "./providers/oauth/config.js";
export {
  type OAuthDataFor,
  type OAuthProviderName,
  type OAuthTokensByProvider,
} from "./providers/oauth/data.js";
export {
  createOAuthProviderRouter,
  type OAuthDependencies,
} from "./providers/oauth/handler.js";
export {
  createOneTimeCodeStore,
  setupOneTimeCodeRoute,
  type OneTimeCodeRouteDependencies,
  type OneTimeCodeStore,
} from "./providers/oauth/oneTimeCode.js";
export {
  defineProvider,
  type DefinedOAuthProvider,
} from "./providers/oauth/provider.js";
export {
  type OAuthProviderAuthConfig,
  type ProviderProfile,
} from "./providers/oauth/providers/common.js";
export { createDiscordAuthProvider } from "./providers/oauth/providers/discord.js";
export { createGitHubAuthProvider } from "./providers/oauth/providers/github.js";
export { createGoogleAuthProvider } from "./providers/oauth/providers/google.js";
export {
  createKeycloakAuthProvider,
  type KeycloakAuthConfig,
} from "./providers/oauth/providers/keycloak.js";
export { createMicrosoftAuthProvider } from "./providers/oauth/providers/microsoft.js";
export { createSlackAuthProvider } from "./providers/oauth/providers/slack.js";
export {
  createOAuthRedirects,
  type OAuthRedirects,
  type OAuthRedirectsConfig,
} from "./providers/oauth/redirects.js";
export {
  createUsernameAuthProvider,
  type UsernameAuthConfig,
  type UsernameAuthDependencies,
} from "./providers/username/provider.js";
export {
  createAuthProvidersRouter,
  createLogoutHandler,
  createMeHandler,
} from "./routes.js";
export {
  createSessions,
  type LuciaPrismaModel,
  type SessionAndUser,
  type Sessions,
  type SessionsConfig,
} from "./sessions.js";
