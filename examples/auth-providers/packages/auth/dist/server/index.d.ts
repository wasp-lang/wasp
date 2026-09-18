import type { ServerAuthHandlerFactory } from "@wasp.sh/auth-contract";
import { isEmailResendAllowed, type EmailHelpers } from "./email/utils.js";
import type { WaspAuthOptions } from "./types.js";
/**
 * Wasp's own authentication as an auth handler package.
 *
 * Wasp instantiates this exactly like any handler package: with the runtime
 * window (the credentials facet, plus the `email-send` grant when the email
 * method is on), the serializable options the
 * spec helper captured, and the user-code extensions the manifest
 * referenced, delivered through virtual modules. The route handler mounts
 * at `/auth/<scheme>`.
 */
export declare const createServerAuthHandler: ServerAuthHandlerFactory<WaspAuthOptions, never, true>;
export declare const createEmailVerificationLink: EmailHelpers["createEmailVerificationLink"];
export declare const createPasswordResetLink: EmailHelpers["createPasswordResetLink"];
export declare const sendEmailVerificationEmail: EmailHelpers["sendEmailVerificationEmail"];
export declare const sendPasswordResetEmail: EmailHelpers["sendPasswordResetEmail"];
export { isEmailResendAllowed };
export { hashPassword, verifyPassword } from "@wasp.sh/lib-auth/node";
export { getEmail, getUsername } from "../user.js";
export { HttpError } from "./http.js";
export type { EmailContent, GetPasswordResetEmailContentFn, GetVerificationEmailContentFn, OAuthData, OAuthProviderName, OnAfterEmailVerifiedHook, OnBeforeOAuthRedirectHook, WaspAuthExtensions, WaspAuthOptions, WaspAuthRuntime, } from "./types.js";
export { ensurePasswordIsPresent, ensureTokenIsPresent, ensureValidEmail, ensureValidPassword, ensureValidUsername, } from "./validation.js";
