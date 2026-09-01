import type { ServerAdapterFactory } from "@wasp.sh/auth-contract";
import type { WaspAuthLibOptions } from "./spec.js";
/**
 * Wasp's own auth, lifted out of the compiler's templates into an ordinary
 * auth provider package: the username & password method, the email method
 * (verification links, password reset) and Google OAuth, all running on
 * nothing but the public auth provider contract.
 *
 * Where each in-tree power comes from out here:
 *
 * - Identity storage: `runtime.identityNamespaces(...)`, one namespace per
 *   method (`external:wasp-auth/username`, `/email`, `/google`), password
 *   hashes in the sealed `secrets` channel.
 * - Password hashing and JWTs: `@wasp.sh/lib-auth/node` -- the same
 *   `hashPassword`/`verifyPassword`/`createJWTHelpers` the in-tree flows use,
 *   so stored hashes and token mechanics are format-identical.
 * - Sessions: the `wasp-sessions` grant. Logins mint through
 *   `runtime.sessions.issue` (the session records this provider's id);
 *   password reset revokes through `runtime.sessions.revokeAllForSubject`.
 * - Email: the `email-send` grant -- the app's configured emailSender, with
 *   the manifest-level guarantee that the app HAS one.
 * - OAuth state/PKCE cookies: `runtime.isDevelopment` drives the `secure`
 *   flag, exactly like the in-tree `oauth/cookies.ts`.
 * - The one-time-code handback: this package's own short-lived JWT
 *   (`WASP_AUTH_TOKENS_SECRET`, NOT the framework's reserved `JWT_SECRET`),
 *   redeemed at `POST /wasp-auth/exchange-code` into a minted session the
 *   client adopts via its `setSession` sink.
 */
type Grants = "wasp-sessions" | "identity-namespaces";
export declare const createServerAdapter: ServerAdapterFactory<WaspAuthLibOptions, Grants>;
export {};
