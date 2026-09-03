{{={= =}=}}
import { Request as ExpressRequest } from "express";

import { type AuthUserData } from '../../auth/user.js';

import { canRevokeSessions, type AuthProvider, type VerifiedSession } from "./provider/types.js";
import { getAuthProvider, authProviders } from "./provider/index.js";
import * as sessionStore from "./sessionStore.js";

import { prisma } from '../index.js';
import { createAuthUserData } from "../../auth/user.js";
import type { AuthProviderId } from '../../auth/provider.js';
import { getIdentityStore } from './identityStore.js';
import { findAuthWithUserBy, validateAndGetUserFields, type ProviderId } from './utils.js';
import {
  fireVetoableHook,
  onAfterLoginHook,
  onAfterSignupHook,
  onBeforeLoginHook,
  onBeforeSignupHook,
} from './hookDispatch.js';
{=# authProviders =}
{=# userSignupFields.isDefined =}
{=& userSignupFields.importStatement =}
{=/ userSignupFields.isDefined =}
{=/ authProviders =}

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

// PRIVATE API
export type SessionAndUser = {
  sessionId: string;
  user: AuthUserData;
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
  return loadSessionAndUser(session.id, session.authId, session.providerId);
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
async function loadSessionAndUser(sessionId: string, authId: string, sessionProviderId: string): Promise<SessionAndUser | null> {
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

  return { sessionId, user: createAuthUserData(user, sessionProviderId) };
}

// The provider's `userSignupFields` compute a provisioned user's own fields
// from the claims that provider verified. Each provider brings its own.
const userSignupFieldsByProviderId: Partial<Record<AuthProviderId, unknown>> = {
  {=# authProviders =}
  '{= providerId =}': {=# userSignupFields.isDefined =}{= userSignupFields.importIdentifier =}{=/ userSignupFields.isDefined =}{=^ userSignupFields.isDefined =}undefined{=/ userSignupFields.isDefined =},
  {=/ authProviders =}
}

// PRIVATE API
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
export async function exchangeRequestForSession(
  providerId: AuthProviderId,
  req: ExpressRequest,
): Promise<{ id: string } | null> {
  const provider: AuthProvider | undefined =
    (authProviders as Record<string, AuthProvider>)[providerId];
  if (provider === undefined) {
    return null;
  }
  // A provider that throws instead of returning `unauthenticated` must still
  // produce a 401, not a 500: whether a bad credential is rejected by return
  // value or by exception is the adapter's internal business, and neither
  // shape may leak provider internals to the caller.
  const result = await Promise.resolve()
    .then(() => provider.authenticate(toWebRequest(req)))
    .catch((error) => {
      console.error(`Auth provider '${providerId}' threw while authenticating:`, error);
      return { status: 'unauthenticated' } as const;
    });
  if (result.status !== 'authenticated') {
    return null;
  }

  const { sessionId: providerSessionId, subjectId, claims } = result.session;
  const authId = await resolveExternalSubject(providerId, subjectId, claims, undefined, undefined, req);
  if (authId === null) {
    return null;
  }

  // The app's login hooks fire around every exchange mint -- this choke point
  // is what makes the app's login veto cover external providers too.
  const auth = await findAuthWithUserBy({ id: authId });
  if (auth === null) {
    return null;
  }
  await fireVetoableHook(() =>
    onBeforeLoginHook({
      req,
      providerId: makeHookProviderId(providerId, subjectId),
      user: auth.user,
    }),
  );

  // A stateless verifier returns no provider session id -- then there is
  // nothing to revoke upstream at logout and Wasp's session stands alone.
  const session = await sessionStore.createSession(authId, {
    providerId,
    providerSessionId,
  });

  await onAfterLoginHook({
    req,
    providerId: makeHookProviderId(providerId, subjectId),
    user: auth.user,
  });

  return session;
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
  providerId: AuthProviderId,
  subjectId: string,
  claims: VerifiedSession['claims'],
  identity?: {
    data?: Record<string, unknown>;
    secrets?: Record<string, unknown>;
  },
  // The identity namespace to record under; callers with the
  // 'identity-namespaces' grant multiplex several, everyone else records
  // under the provider id. The runtime guards membership before we get here.
  namespace: string = providerId,
  req?: ExpressRequest,
): Promise<string | null> {
  const identities = getIdentityStore(namespace);

  const existing = await identities.find(subjectId);
  if (existing) {
    return existing.authId;
  }

  // A brand-new subject IS a signup, so the app's signup hooks fire here --
  // this choke point is what makes the app's veto cover external providers
  // too. The veto runs BEFORE the `userSignupFields` getters.
  await fireVetoableHook(() =>
    onBeforeSignupHook({
      req,
      providerId: makeHookProviderId(namespace, subjectId),
    }),
  );

  // The provider's `userSignupFields` compute the new user's own fields from
  // the claims the provider verified -- the only way a user entity with
  // required columns can be provisioned at all. Computed only for brand-new
  // subjects.
  const userFields = await computeProviderUserFields(providerId, claims);

  let created;
  try {
    created = await identities.createIdentity(
      subjectId,
      {
        // The provider-verified profile data (email, name, ...) as of the
        // moment this subject was first seen. Wasp-written and read-only
        // afterwards, so its provenance can be trusted.
        claims: { ...(claims ?? {}) },
        data: identity?.data,
        secrets: identity?.secrets,
      },
      // Using `any` to defer validation of required-but-unset fields to
      // Prisma, which reports them precisely.
      userFields as any,
    );
  } catch (e: unknown) {
    // Another request provisioned the same subject between our read and our
    // write. Its row is the winner; the loser re-reads and, deliberately, does
    // NOT fire onAfterSignup -- one signup, one hook firing.
    if (isUniqueConstraintViolation(e)) {
      const raced = await identities.find(subjectId);
      return raced === null ? null : raced.authId;
    }
    throw e;
  }

  await onAfterSignupHook({
    req,
    providerId: makeHookProviderId(namespace, subjectId),
    user: created,
  });

  return created.{= authFieldOnUserEntityName =}!.id;
}

function isUniqueConstraintViolation(e: unknown): boolean {
  return (
    typeof e === 'object' && e !== null && 'code' in e && (e as { code: unknown }).code === 'P2002'
  );
}

function makeHookProviderId(namespace: string, subjectId: string): ProviderId {
  return { providerName: namespace, providerUserId: subjectId };
}

// PRIVATE API
/**
 * Eager provisioning: the runtime channel an in-process adapter calls when it
 * observes its own signup, so the local user exists from that moment instead
 * of from the first login exchange. Same code path as the exchange's
 * provisioning, called sooner -- idempotent by the same unique constraint.
 */
export async function provisionAuthUser(
  providerId: AuthProviderId,
  subjectId: string,
  claims: VerifiedSession['claims'],
  identity?: {
    data?: Record<string, unknown>;
    secrets?: Record<string, unknown>;
  },
  namespace?: string,
): Promise<{ authId: string } | null> {
  const authId = await resolveExternalSubject(providerId, subjectId, claims, identity, namespace);
  return authId === null ? null : { authId };
}

// PRIVATE API
/**
 * Runs the provider's manifest-level `userSignupFields` over verified claims,
 * producing the user entity's own fields. Shared by just-in-time provisioning
 * and the identity facet's `create` (when no field getter is passed).
 */
export async function computeProviderUserFields(
  providerId: AuthProviderId,
  claims: VerifiedSession['claims'],
): Promise<Record<string, unknown>> {
  return validateAndGetUserFields(
    { ...(claims ?? {}) },
    userSignupFieldsByProviderId[providerId] as any,
  );
}

// PRIVATE API
/**
 * Dual sign-out, ASP.NET Core style: Wasp's session is always revoked, and when
 * its minting provider can revoke its own session, that one is revoked too.
 * The session row recorded which provider minted it, so the revocation always
 * goes to the right provider. The local revocation is what logs the user out;
 * the upstream one is best-effort -- its failure is logged, never surfaced, so
 * logout cannot be blocked by a provider outage.
 */
export async function invalidateSession(sessionId: string): Promise<void> {
  const stored = await sessionStore.getStoredSession(sessionId);
  await sessionStore.revokeSession(sessionId);

  if (stored?.providerSessionId != null && stored.providerId != null) {
    const provider = getAuthProvider(stored.providerId);
    if (provider !== undefined && canRevokeSessions(provider)) {
      try {
        await provider.revokeSession(stored.providerSessionId);
      } catch (error) {
        console.error(
          'Wasp session revoked, but revoking the auth provider session failed:',
          error,
        );
      }
    }
  }
}

// PRIVATE API
// Invalidates all of the auth entity's sessions, upstream ones included where
// each session's minting provider can revoke (same best-effort semantics as
// `invalidateSession`).
export async function invalidateAllSessionsForAuthId(authId: string): Promise<void> {
  const stored = await sessionStore.getStoredSessionsForAuthId(authId);
  await sessionStore.revokeAllSessions(authId);

  for (const session of stored) {
    if (session.providerSessionId == null || session.providerId == null) {
      continue;
    }
    const provider = getAuthProvider(session.providerId);
    if (provider === undefined || !canRevokeSessions(provider)) {
      continue;
    }
    try {
      await provider.revokeSession(session.providerSessionId);
    } catch (error) {
      console.error(
        'Wasp session revoked, but revoking the auth provider session failed:',
        error,
      );
    }
  }
}
