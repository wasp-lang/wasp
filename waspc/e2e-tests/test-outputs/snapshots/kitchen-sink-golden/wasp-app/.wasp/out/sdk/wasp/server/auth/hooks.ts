import type * as AuthCore from "@wasp.sh/lib-auth/node";
import type { AppAuthEntityTypes } from "../../auth/user.js";
import type { prisma } from "../index.js";

// PRIVATE API (used in the SDK and the server)
/**
 * The app's types that the auth lib's hook types are instantiated with.
 */
export type AuthHookTypes = {
  prisma: typeof prisma;
  entities: AppAuthEntityTypes;
  oauthData: OAuthData;
};

// PUBLIC API
export type OnBeforeSignupHook = AuthCore.OnBeforeSignupHook<AuthHookTypes>;

// PUBLIC API
export type OnAfterSignupHook = AuthCore.OnAfterSignupHook<AuthHookTypes>;

// PUBLIC API
/**
 * Use this type for typing your `onAfterEmailVerified` hook.
 *
 * Wasp calls this hook exactly once, after a user successfully verifies their email during the email verification flow.
 *
 * @example
 * ```ts
 * export const onAfterEmailVerified: OnAfterEmailVerifiedHook = async ({
 *   email,
 * }) => {
 *   await emailSender.send({
 *     to: email,
 *     subject: 'Welcome to our platform!',
 *     text: `Your email ${email} has been verified successfully.`
 *   })
 * }
 * ```
 */
export type OnAfterEmailVerifiedHook =
  AuthCore.OnAfterEmailVerifiedHook<AuthHookTypes>;

// PUBLIC API
/**
 * @returns Object with a URL that the OAuth flow should redirect to.
 */
export type OnBeforeOAuthRedirectHook =
  AuthCore.OnBeforeOAuthRedirectHook<AuthHookTypes>;

// PUBLIC API
export type OnBeforeLoginHook = AuthCore.OnBeforeLoginHook<AuthHookTypes>;

// PUBLIC API
export type OnAfterLoginHook = AuthCore.OnAfterLoginHook<AuthHookTypes>;

// PRIVATE API (used in the SDK and the server)
export type InternalAuthHookParams =
  AuthCore.InternalAuthHookParams<AuthHookTypes>;

// PUBLIC API
export type OAuthData = AuthCore.OAuthDataFor<
  | "slack"
  | "discord"
  | "google"
  | "github"
  | "microsoft"
  | never
>;
