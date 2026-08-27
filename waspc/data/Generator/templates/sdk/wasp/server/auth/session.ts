{{={= =}=}}
import { Request as ExpressRequest } from "express";

import { type AuthUserData } from '../../auth/user.js';

import { authProvider } from "./provider/index.js";
import { canRevokeSessions{=# isCustomAuthProviderUsed =}, type VerifiedSession{=/ isCustomAuthProviderUsed =} } from "./provider/types.js";
import * as sessionStore from "./sessionStore.js";

import { prisma } from '../index.js';
import { createAuthUserData } from "../../auth/user.js";
{=# isCustomAuthProviderUsed =}
import { getIdentityStore } from './identityStore.js';
import { validateAndGetUserFields } from './utils.js';
{=# externalUserSignupFields.isDefined =}
{=& externalUserSignupFields.importStatement =}
{=/ externalUserSignupFields.isDefined =}
{=/ isCustomAuthProviderUsed =}

/**
 * Wasp's session layer.
 *
 * Every request is authenticated against a session Wasp itself minted, whichever
 * provider verified the login -- the classic full-stack-framework model (Rails,
 * Django, ASP.NET Core). An external provider is consulted exactly twice: once at
 * login, when `POST /auth/login` exchanges its credential for a Wasp session, and
 * once at logout, when the provider's own session is revoked alongside Wasp's
 * (dual sign-out, same as ASP.NET Core's two-scheme `SignOut`).
 *
 * Known and accepted gap: revocation on the provider's side does NOT end the Wasp
 * session -- it lives until it expires or the user logs out. This is the same
 * trade-off ASP.NET Core's cookie makes after an OIDC login.
 */

// PRIVATE API
export type SessionAndUser = {
  sessionId: string;
  user: AuthUserData;
}

// PRIVATE API
// Creates a new session for the `authId` in the database.
export async function createSession(authId: string): Promise<{ id: string }> {
  return sessionStore.createSession(authId);
}

// PRIVATE API
export async function getSessionAndUserFromBearerToken(req: ExpressRequest): Promise<SessionAndUser | null> {
  const token = sessionStore.getBearerToken(req.headers.authorization);
  return token === null ? null : getSessionAndUserFromSessionId(token);
}

// PRIVATE API
// Authenticates a bare session token with no surrounding request -- websockets
// hand us a token out of `socket.handshake.auth` rather than an HTTP request.
export async function getSessionAndUserFromSessionId(sessionId: string): Promise<SessionAndUser | null> {
  const session = await sessionStore.validateSession(sessionId);
  if (session === null) {
    return null;
  }
  return loadSessionAndUser(session.id, session.authId);
}

/**
 * Turns a validated session into the user data Wasp exposes as `context.user`.
 *
 * This is the step that guarantees `context.user` is always the developer's own
 * `{= userEntityUpper =}` entity, whichever provider vouched for the login.
 *
 * We look the user up *through* the auth entity rather than by its own id, which
 * keeps this to a single query.
 */
async function loadSessionAndUser(sessionId: string, authId: string): Promise<SessionAndUser | null> {
  const user = await prisma.{= userEntityLower =}.findFirst({
    where: { {= authFieldOnUserEntityName =}: { id: authId } },
    include: {
      {= authFieldOnUserEntityName =}: {
        include: {
          {= identitiesFieldOnAuthEntityName =}: true
        }
      }
    }
  });

  // An auth entity that isn't linked to a user can't identify anyone, so we treat
  // the session as unauthenticated rather than erroring.
  if (!user) {
    return null;
  }

  return { sessionId, user: createAuthUserData(user) };
}

{=# isCustomAuthProviderUsed =}
// PRIVATE API
/**
 * The `POST /auth/login` exchange: verifies the provider credential carried in
 * the request, provisions the local user if this is the first time we see the
 * subject, and mints the Wasp session all subsequent requests authenticate with.
 *
 * The provider's own session id is stored on the Wasp session so logout can
 * revoke both (dual sign-out). After this point the provider is off the request
 * hot path entirely.
 */
export async function exchangeRequestForSession(req: ExpressRequest): Promise<{ id: string } | null> {
  const result = await authProvider.authenticate(toWebRequest(req));
  if (result.status !== 'authenticated') {
    return null;
  }

  const { sessionId: providerSessionId, subjectId, claims } = result.session;
  const authId = await resolveExternalSubject(subjectId, claims);
  if (authId === null) {
    return null;
  }

  // A stateless verifier returns no provider session id -- then there is
  // nothing to revoke upstream at logout and Wasp's session stands alone.
  return sessionStore.createSession(
    authId,
    providerSessionId === undefined ? undefined : { providerSessionId },
  );
}

/**
 * Providers speak standard web `Request`, not Express -- an external provider's
 * SDK (Clerk's, Better Auth's) natively consumes one, and it keeps the contract
 * free of Express. This is the single place an Express request is converted.
 */
function toWebRequest(req: ExpressRequest): Request {
  const headers = new Headers();
  for (const [key, value] of Object.entries(req.headers)) {
    if (typeof value === 'string') {
      headers.set(key, value);
    } else if (Array.isArray(value)) {
      headers.set(key, value.join(', '));
    }
  }

  const host = req.get('host') ?? 'localhost';
  return new Request(`${req.protocol}://${host}${req.originalUrl}`, {
    method: req.method,
    headers,
  });
}

/**
 * Maps a provider-owned subject id onto Wasp's auth entity, creating the local
 * rows the first time we see that subject.
 *
 * This is what keeps `context.user` honest. RedwoodJS shipped nine auth adapters
 * over one interface but left provisioning to the developer, so `currentUser.id`
 * ended up meaning a database id under one adapter and a provider's opaque string
 * under another. Wasp always resolves to a row in the developer's own entity.
 *
 * The upsert is deliberately written as create-then-handle-conflict rather than
 * find-then-create: two requests can arrive for the same brand-new subject at the
 * same time, and only the unique constraint can settle that race.
 */
async function resolveExternalSubject(
  subjectId: string,
  claims: VerifiedSession['claims'],
  identity?: {
    data?: Record<string, unknown>;
    secrets?: Record<string, unknown>;
  },
): Promise<string | null> {
  const identities = getIdentityStore(authProvider.id);

  const existing = await identities.find(subjectId);
  if (existing) {
    return existing.authId;
  }

  // The app's `userSignupFields` compute the new user's own fields from the
  // claims the provider verified -- the only way a user entity with required
  // columns can be provisioned at all. Computed only for brand-new subjects.
  const userFields = await validateAndGetUserFields(
    { ...(claims ?? {}) },
    {=# externalUserSignupFields.isDefined =}{= externalUserSignupFields.importIdentifier =}{=/ externalUserSignupFields.isDefined =}{=^ externalUserSignupFields.isDefined =}undefined{=/ externalUserSignupFields.isDefined =},
  );

  // `provision` is the store's idempotent create: a concurrent request for the
  // same brand-new subject is settled by the unique constraint, and the loser
  // returns the winner's row.
  const provisioned = await identities.provision(
    subjectId,
    {
      // The provider-verified profile data (email, name, ...) as of the moment
      // this subject was first seen. Wasp-written and read-only afterwards, so
      // its provenance can be trusted.
      claims: { ...(claims ?? {}) },
      data: identity?.data,
      secrets: identity?.secrets,
    },
    // Using `any` to defer validation of required-but-unset fields to Prisma,
    // which reports them precisely.
    userFields as any,
  );
  return provisioned?.authId ?? null;
}

// PRIVATE API
/**
 * Eager provisioning: the runtime channel an in-process adapter calls when it
 * observes its own signup, so the local user exists from that moment instead
 * of from the first login exchange. Same code path as the exchange's
 * provisioning, called sooner -- idempotent by the same unique constraint.
 */
export async function provisionAuthUser(
  subjectId: string,
  claims: VerifiedSession['claims'],
  identity?: {
    data?: Record<string, unknown>;
    secrets?: Record<string, unknown>;
  },
): Promise<{ authId: string } | null> {
  const authId = await resolveExternalSubject(subjectId, claims, identity);
  return authId === null ? null : { authId };
}
{=/ isCustomAuthProviderUsed =}

// PRIVATE API
/**
 * Dual sign-out, ASP.NET Core style: Wasp's session is always revoked, and when
 * it was minted from an external provider's credential the provider's own
 * session is revoked too (when the provider is able to). The local revocation
 * is what logs the user out; the upstream one is best-effort -- its failure is
 * logged, never surfaced, so logout cannot be blocked by a provider outage.
 */
export async function invalidateSession(sessionId: string): Promise<void> {
  const stored = await sessionStore.getStoredSession(sessionId);
  await sessionStore.revokeSession(sessionId);

  if (stored?.providerSessionId != null && canRevokeSessions(authProvider)) {
    try {
      await authProvider.revokeSession(stored.providerSessionId);
    } catch (error) {
      console.error(
        'Wasp session revoked, but revoking the auth provider session failed:',
        error,
      );
    }
  }
}

// PRIVATE API
// Invalidates all of the auth entity's sessions, upstream ones included where
// the provider can revoke (same best-effort semantics as `invalidateSession`).
export async function invalidateAllSessionsForAuthId(authId: string): Promise<void> {
  const stored = await sessionStore.getStoredSessionsForAuthId(authId);
  await sessionStore.revokeAllSessions(authId);

  if (!canRevokeSessions(authProvider)) {
    return;
  }
  for (const session of stored) {
    if (session.providerSessionId != null) {
      try {
        await authProvider.revokeSession(session.providerSessionId);
      } catch (error) {
        console.error(
          'Wasp session revoked, but revoking the auth provider session failed:',
          error,
        );
      }
    }
  }
}
