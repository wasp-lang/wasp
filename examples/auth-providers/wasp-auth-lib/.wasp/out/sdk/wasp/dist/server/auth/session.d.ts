import { Request as ExpressRequest } from "express";
import { type AuthUserData } from '../../auth/user.js';
import { type VerifiedSession } from "./provider/types.js";
import type { AuthProviderId, ExternalAuthProviderId } from '../../auth/provider.js';
/**
 * Wasp's session layer.
 *
 * Every request is authenticated against a session Wasp itself minted, whichever
 * provider verified the login -- the classic full-stack-framework model (Rails,
 * Django, ASP.NET Core). A provider is consulted exactly twice: once at login,
 * when `POST /auth/login/:providerId` exchanges its credential for a Wasp
 * session, and once at logout, when the provider's own session is revoked
 * alongside Wasp's (dual sign-out, same as ASP.NET Core's two-scheme `SignOut`).
 *
 * Every session records the id of the provider that minted it, so logout and
 * user code always know which provider vouched for the login without ever
 * asking the providers.
 *
 * Known and accepted gap: revocation on the provider's side does NOT end the Wasp
 * session -- it lives until it expires or the user logs out. This is the same
 * trade-off ASP.NET Core's cookie makes after an OIDC login.
 */
export type SessionAndUser = {
    sessionId: string;
    user: AuthUserData;
};
export declare function getSessionAndUserFromBearerToken(req: ExpressRequest): Promise<SessionAndUser | null>;
export declare function getSessionAndUserFromSessionId(sessionId: string): Promise<SessionAndUser | null>;
/**
 * The `POST /auth/login/:providerId` exchange: verifies the provider
 * credential carried in the request against the *addressed* provider,
 * provisions the local user if this is the first time we see the subject, and
 * mints the Wasp session all subsequent requests authenticate with.
 *
 * The addressed provider rejecting the credential is final -- the exchange
 * never falls through to another provider, so which identity a credential
 * becomes can never depend on configuration order.
 *
 * The provider's own session id and the provider's id are stored on the Wasp
 * session so logout can revoke both (dual sign-out) against the right
 * provider. After this point the provider is off the request hot path
 * entirely.
 */
export declare function exchangeRequestForSession(providerId: ExternalAuthProviderId, req: ExpressRequest): Promise<{
    id: string;
} | null>;
/**
 * Eager provisioning: the runtime channel an in-process adapter calls when it
 * observes its own signup, so the local user exists from that moment instead
 * of from the first login exchange. Same code path as the exchange's
 * provisioning, called sooner -- idempotent by the same unique constraint.
 */
export declare function provisionAuthUser(providerId: AuthProviderId, subjectId: string, claims: VerifiedSession['claims'], identity?: {
    data?: Record<string, unknown>;
    secrets?: Record<string, unknown>;
}, namespace?: string): Promise<{
    authId: string;
} | null>;
/**
 * Runs the provider's manifest-level `userSignupFields` over verified claims,
 * producing the user entity's own fields. Shared by just-in-time provisioning
 * and the identity facet's `create` (when no field getter is passed).
 */
export declare function computeProviderUserFields(providerId: AuthProviderId, claims: VerifiedSession['claims']): Promise<Record<string, unknown>>;
/**
 * Dual sign-out, ASP.NET Core style: Wasp's session is always revoked, and when
 * its minting provider can revoke its own session, that one is revoked too.
 * The session row recorded which provider minted it, so the revocation always
 * goes to the right provider. The local revocation is what logs the user out;
 * the upstream one is best-effort -- its failure is logged, never surfaced, so
 * logout cannot be blocked by a provider outage.
 */
export declare function invalidateSession(sessionId: string): Promise<void>;
export declare function invalidateAllSessionsForAuthId(authId: string): Promise<void>;
//# sourceMappingURL=session.d.ts.map