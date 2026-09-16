{{={= =}=}}
import type { Request as ExpressRequest } from "express";

import type { AuthUserData } from '../../auth/user.js';

import type { AuthHandler, Principal } from "./handler/types.js";
import { authSchemes, defaultScheme } from "./schemes.js";
import { toWebRequest } from "./issuer.js";

import { prisma } from '../index.js';
import { createAuthUserData } from "../../auth/user.js";
import type { AuthSchemeName } from '../../auth/scheme.js';
import { getIdentityStore } from './identityStore.js';
import { validateAndGetUserFields, type ProviderId } from './utils.js';
import {
  fireVetoableHook,
  onAfterSignupHook,
  onBeforeSignupHook,
} from './hookDispatch.js';
{=# schemes =}
{=# userSignupFields.isDefined =}
{=& userSignupFields.importStatement =}
{=/ userSignupFields.isDefined =}
{=/ schemes =}

/**
 * Wasp's request authentication and provisioning layer.
 *
 * Every guarded request is authenticated by one of the app's schemes: the
 * default one, or the ones an asset lists. Whatever scheme answers, the
 * result is always the app's own `{= userEntityUpper =}` row -- provisioning
 * is the framework's job, and a scheme only ever names a subject.
 */

// PRIVATE API
export type AuthenticatedRequest = {
  scheme: AuthSchemeName;
  credentialId?: string;
  user: AuthUserData;
}

// PRIVATE API
/**
 * Tries the listed schemes in order; the first one that authenticates wins.
 * No merging: the request is exactly one scheme's subject.
 */
export async function authenticateRequest(
  req: ExpressRequest,
  schemeNames: readonly AuthSchemeName[],
): Promise<AuthenticatedRequest | null> {
  return authenticateWebRequest(toWebRequest(req), schemeNames);
}

// PRIVATE API
// Authenticates a bare bearer credential with no surrounding request --
// websockets hand us one out of `socket.handshake.auth` rather than an HTTP
// request. Only the default scheme is consulted.
export async function authenticateCredential(credential: string): Promise<AuthenticatedRequest | null> {
  const request = new Request('http://wasp.local/', {
    headers: { Authorization: `Bearer ${credential}` },
  });
  return authenticateWebRequest(request, [defaultScheme]);
}

async function authenticateWebRequest(
  request: Request,
  schemeNames: readonly AuthSchemeName[],
): Promise<AuthenticatedRequest | null> {
  for (const scheme of schemeNames) {
    const handler: AuthHandler = authSchemes[scheme];
    // A handler that throws instead of returning `unauthenticated` must not
    // take the request down: whether a bad credential is rejected by return
    // value or by exception is the handler's internal business.
    const result = await Promise.resolve()
      .then(() => handler.authenticate(request))
      .catch((error) => {
        console.error(`Auth scheme '${scheme}' threw while authenticating:`, error);
        return { status: 'unauthenticated' } as const;
      });
    if (result.status !== 'authenticated') {
      continue;
    }
    const user = await loadUserForPrincipal(scheme, result.principal);
    if (user === null) {
      continue;
    }
    return { scheme, credentialId: result.principal.credentialId, user };
  }
  return null;
}

/**
 * Turns an authenticated principal into the user data Wasp exposes as
 * `context.user`. A Wasp-issued credential names the Auth entity directly;
 * any other scheme names its own subject, which is resolved through the
 * identity store -- provisioning the local user on first sight.
 */
async function loadUserForPrincipal(scheme: AuthSchemeName, principal: Principal): Promise<AuthUserData | null> {
  const isWaspCredential = principal.signedInBy !== undefined && principal.namespace === undefined && principal.credentialId !== undefined && isAuthEntityId(principal.subjectId);
  const authId = isWaspCredential
    ? principal.subjectId
    : await resolveSubject(scheme, principal.subjectId, principal.claims, undefined, principal.namespace ?? scheme);
  if (authId === null) {
    return null;
  }
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
  // the request as unauthenticated rather than erroring.
  if (!user) {
    return null;
  }

  return createAuthUserData(user, scheme, principal.signedInBy ?? scheme);
}

// Wasp-issued credentials carry the Auth entity's uuid as their subject; a
// handler's own subject ids are whatever the provider uses.
function isAuthEntityId(subjectId: string): boolean {
  return /^[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}$/i.test(subjectId);
}

// The scheme's `userSignupFields` compute a provisioned user's own fields
// from the claims that scheme verified. Each scheme brings its own.
const userSignupFieldsByScheme: Partial<Record<AuthSchemeName, unknown>> = {
  {=# schemes =}
  '{= schemeName =}': {=# userSignupFields.isDefined =}{= userSignupFields.importIdentifier =}{=/ userSignupFields.isDefined =}{=^ userSignupFields.isDefined =}undefined{=/ userSignupFields.isDefined =},
  {=/ schemes =}
}

/**
 * Maps a scheme-owned subject id onto Wasp's auth entity, creating the local
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
async function resolveSubject(
  scheme: AuthSchemeName,
  subjectId: string,
  claims: Principal['claims'],
  identity?: {
    data?: Record<string, unknown>;
    secrets?: Record<string, unknown>;
  },
  // The identity namespace to record under; callers with the
  // 'identity-namespaces' grant multiplex several, everyone else records
  // under the scheme name. The runtime guards membership before we get here.
  namespace: string = scheme,
  req?: ExpressRequest,
): Promise<string | null> {
  const identities = getIdentityStore(namespace);

  const existing = await identities.find(subjectId);
  if (existing) {
    return existing.authId;
  }

  // A brand-new subject IS a signup, so the app's signup hooks fire here --
  // this choke point is what makes the app's veto cover every scheme. The
  // veto runs BEFORE the `userSignupFields` getters.
  await fireVetoableHook(() =>
    onBeforeSignupHook({
      req,
      providerId: makeHookProviderId(namespace, subjectId),
    }),
  );

  // The scheme's `userSignupFields` compute the new user's own fields from
  // the claims the handler verified -- the only way a user entity with
  // required columns can be provisioned at all. Computed only for brand-new
  // subjects.
  const userFields = await computeSchemeUserFields(scheme, claims);

  let created;
  try {
    created = await identities.createIdentity(
      subjectId,
      {
        // The handler-verified profile data (email, name, ...) as of the
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
 * Eager provisioning: the runtime channel an in-process handler calls when it
 * observes its own signup, so the local user exists from that moment instead
 * of from the first authenticated request. Same code path as just-in-time
 * provisioning, called sooner -- idempotent by the same unique constraint.
 */
export async function provisionAuthUser(
  scheme: AuthSchemeName,
  subjectId: string,
  claims: Principal['claims'],
  identity?: {
    data?: Record<string, unknown>;
    secrets?: Record<string, unknown>;
  },
  namespace?: string,
): Promise<{ authId: string } | null> {
  const authId = await resolveSubject(scheme, subjectId, claims, identity, namespace);
  return authId === null ? null : { authId };
}

// PRIVATE API
/**
 * Runs the scheme's manifest-level `userSignupFields` over verified claims,
 * producing the user entity's own fields. Shared by just-in-time provisioning
 * and the identity facet's `create` (when no field getter is passed).
 */
export async function computeSchemeUserFields(
  scheme: AuthSchemeName,
  claims: Principal['claims'],
): Promise<Record<string, unknown>> {
  return validateAndGetUserFields(
    { ...(claims ?? {}) },
    userSignupFieldsByScheme[scheme] as any,
  );
}
