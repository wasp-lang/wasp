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
import { oauthRoutes } from "./oauth/index.js";
import type {
  Ctx,
  WaspAuthExtensions,
  WaspAuthGrants,
  WaspAuthOptions,
} from "./types.js";
import { usernameRoutes } from "./username.js";

/**
 * Wasp's own authentication as an auth provider package.
 *
 * The compiler instantiates this exactly like any adapter package: with the
 * runtime window (`wasp-sessions` and `identity-namespaces` grants, plus
 * `email-send` when the email method is on), the serializable options it
 * derives from `waspAuth({ ... })`, and the user-code extensions delivered
 * through virtual modules. The route handler mounts at `/auth`, after
 * Wasp's own `/auth/me`, `/auth/logout` and `/auth/login/:providerId`.
 */
export const createServerAdapter: ServerAdapterFactory<
  WaspAuthOptions,
  WaspAuthGrants
> = (runtime, options, extensions): ServerAdapter => {
  const ctx: Ctx = {
    runtime,
    options,
    extensions: (extensions as unknown as WaspAuthExtensions | undefined) ?? {},
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

  // The session read path (bearer Wasp session tokens) is Wasp's own
  // verifier, registered by the compiler under the 'wasp' id; this object
  // only carries the routes.
  const provider: AuthProvider = {
    id: "wasp",
    async authenticate() {
      return { status: "unauthenticated" };
    },
  };

  return { provider, routeHandler: makeDispatcher(routes) };
};

// The email helpers the SDK re-exports as `wasp/server/auth/email`'s public
// API (link builders, senders), bound to the runtime at adapter creation.
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

export { HttpError } from "./http.js";
export type {
  EmailContent,
  GetPasswordResetEmailContentFn,
  GetVerificationEmailContentFn,
  OAuthData,
  OAuthProviderName,
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
