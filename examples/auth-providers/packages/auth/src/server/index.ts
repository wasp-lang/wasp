import type {
  AuthHandler,
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
 * Wasp's own authentication as an auth handler package.
 *
 * Wasp instantiates this exactly like any handler package: with the runtime
 * window (the `identity-namespaces` grant, the credentials facet, plus
 * `email-send` when the email method is on), the serializable options the
 * spec helper captured, and the user-code extensions the manifest
 * referenced, delivered through virtual modules. The route handler mounts
 * at `/auth/<scheme>`.
 */
export const createServerAdapter: ServerAdapterFactory<
  WaspAuthOptions,
  WaspAuthGrants,
  true
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

  // The routes above verify logins; the credential a request carries
  // afterwards belongs to the credentials scheme (this scheme's private
  // issuer by default). Authentication forwards there, the way ASP.NET's
  // remote schemes forward to their sign-in scheme, so `authRequired`
  // naming this scheme recognizes the credentials it handed out.
  const handler: AuthHandler = {
    authenticate: (request) => runtime.credentials.authenticate(request),
    signOut: (request) => runtime.credentials.signOut(request),
  };

  return { handler, routeHandler: makeDispatcher(routes) };
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
