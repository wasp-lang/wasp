import type { ServerAuthAdapterFor } from "@wasp.sh/auth-contract";
import type { waspAuth } from "../spec.js";
import { isEmailResendAllowed, type EmailHelpers } from "./email/utils.js";
/**
 * Wasp's own authentication as an auth handler package.
 *
 * Wasp instantiates this exactly like any handler package: with the runtime
 * window (the credentials facet, plus the `email-send` grant when the email
 * method is on) and the `server.spec` the spec helper captured, with the
 * app's functions live in place. The route handler mounts
 * at `/auth/<scheme>`.
 */
export declare const createServerAuthHandler: ServerAuthAdapterFor<typeof waspAuth>;
export declare const createEmailVerificationLink: EmailHelpers["createEmailVerificationLink"];
export declare const createPasswordResetLink: EmailHelpers["createPasswordResetLink"];
export declare const sendEmailVerificationEmail: EmailHelpers["sendEmailVerificationEmail"];
export declare const sendPasswordResetEmail: EmailHelpers["sendPasswordResetEmail"];
export { isEmailResendAllowed };
export { hashPassword, verifyPassword } from "@wasp.sh/lib-auth/node";
export { getEmail, getUsername } from "../user.js";
export { HttpError } from "./http.js";
export type { EmailContent, GetPasswordResetEmailContentFn, GetVerificationEmailContentFn, OAuthData, OAuthProviderName, OnAfterEmailVerifiedHook, OnBeforeOAuthRedirectHook, WaspAuthRuntime, WaspAuthServerSpec, } from "./types.js";
export { ensurePasswordIsPresent, ensureTokenIsPresent, ensureValidEmail, ensureValidPassword, ensureValidUsername, } from "./validation.js";
