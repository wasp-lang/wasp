import type {
  AuthProvider,
  ServerAdapter,
  ServerAdapterFactory,
} from "@wasp.sh/auth-contract";

import { emailRoutes } from "./email/flows.js";
import {
  isEmailResendAllowed,
  makeEmailHelpers,
  type EmailHelpers,
} from "./email/utils.js";
import { makeDispatcher, type Route } from "./http.js";
import { PROVIDER_ID } from "./namespaces.js";
import { oauthRoutes } from "./oauth/index.js";
import type {
  Ctx,
  OAuthProviderName,
  WaspAuthExtensions,
  WaspAuthGrants,
  WaspAuthOptions,
} from "./types.js";
import { usernameRoutes } from "./username.js";

const OAUTH_PROVIDER_NAMES: OAuthProviderName[] = [
  "google",
  "github",
  "slack",
  "discord",
  "keycloak",
  "microsoft",
];

/**
 * Wasp's own authentication as an auth provider package.
 *
 * Wasp instantiates this exactly like any adapter package: with the runtime
 * window (`wasp-sessions` and `identity-namespaces` grants, plus `email-send`
 * when the email method is on), the serializable options the spec helper
 * captured, and the user-code extensions the manifest referenced, delivered
 * through virtual modules. The route handler mounts at the manifest's
 * basePath (`/auth/wasp`).
 */
export const createServerAdapter: ServerAdapterFactory<
  WaspAuthOptions,
  WaspAuthGrants
> = (runtime, options, extensions): ServerAdapter => {
  const ctx: Ctx = {
    runtime,
    options,
    extensions: groupExtensions(
      (extensions as Record<string, unknown> | undefined) ?? {},
    ),
  };

  const routes: Route[] = [
    ...(options.methods.usernameAndPassword !== undefined
      ? usernameRoutes(ctx)
      : []),
    ...(options.methods.email !== undefined ? emailRoutes(ctx) : []),
    ...oauthRoutes(ctx),
  ];

  if (options.methods.email !== undefined) {
    boundEmailHelpers = makeEmailHelpers(runtime);
  }

  // Sessions are minted from the routes above through the `wasp-sessions`
  // grant, and every request is then authenticated against Wasp's own session
  // store; there is no credential to exchange, so the exchange route answers
  // 'unauthenticated' for this provider.
  const provider: AuthProvider = {
    id: PROVIDER_ID,
    async authenticate() {
      return { status: "unauthenticated" };
    },
  };

  return { provider, routeHandler: makeDispatcher(routes) };
};

/**
 * The manifest delivers user functions as a flat record keyed the way the
 * spec helper named them (`emailUserSignupFields`, `googleConfigFn`, ...);
 * the flows read them grouped by kind.
 */
function groupExtensions(flat: Record<string, unknown>): WaspAuthExtensions {
  const grouped: WaspAuthExtensions = {
    userSignupFields: {},
    configFns: {},
    getVerificationEmailContent: flat.getVerificationEmailContent as never,
    getPasswordResetEmailContent: flat.getPasswordResetEmailContent as never,
    onAfterEmailVerified: flat.onAfterEmailVerified as never,
    onBeforeOAuthRedirect: flat.onBeforeOAuthRedirect as never,
  };
  for (const method of [
    "username",
    "email",
    ...OAUTH_PROVIDER_NAMES,
  ] as const) {
    const fields = flat[`${method}UserSignupFields`];
    if (fields !== undefined) {
      grouped.userSignupFields![method] = fields as never;
    }
  }
  for (const name of OAUTH_PROVIDER_NAMES) {
    const configFn = flat[`${name}ConfigFn`];
    if (configFn !== undefined) {
      grouped.configFns![name] = configFn as never;
    }
  }
  return grouped;
}

// The email helpers (link builders, senders), bound to the runtime at adapter
// creation. User code imports them from `@wasp.sh/auth/server`.
let boundEmailHelpers: EmailHelpers | null = null;

function getEmailHelpers(): EmailHelpers {
  if (boundEmailHelpers === null) {
    throw new Error("Wasp's email auth method is not enabled.");
  }
  return boundEmailHelpers;
}

export const createEmailVerificationLink: EmailHelpers["createEmailVerificationLink"] =
  (...args) => getEmailHelpers().createEmailVerificationLink(...args);
export const createPasswordResetLink: EmailHelpers["createPasswordResetLink"] =
  (...args) => getEmailHelpers().createPasswordResetLink(...args);
export const sendEmailVerificationEmail: EmailHelpers["sendEmailVerificationEmail"] =
  (...args) => getEmailHelpers().sendEmailVerificationEmail(...args);
export const sendPasswordResetEmail: EmailHelpers["sendPasswordResetEmail"] = (
  ...args
) => getEmailHelpers().sendPasswordResetEmail(...args);
export { isEmailResendAllowed };

export { hashPassword, verifyPassword } from "@wasp.sh/lib-auth/node";
export { getEmail, getUsername } from "../user.js";
export { HttpError } from "./http.js";
export type {
  EmailContent,
  GetPasswordResetEmailContentFn,
  GetVerificationEmailContentFn,
  OAuthData,
  OAuthProviderName,
  OnAfterEmailVerifiedHook,
  OnBeforeOAuthRedirectHook,
  WaspAuthExtensions,
  WaspAuthOptions,
  WaspAuthRuntime,
} from "./types.js";
export {
  ensurePasswordIsPresent,
  ensureTokenIsPresent,
  ensureValidEmail,
  ensureValidPassword,
  ensureValidUsername,
} from "./validation.js";
