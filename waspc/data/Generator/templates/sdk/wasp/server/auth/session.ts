{{={= =}=}}
import type { Request as ExpressRequest } from "express";

import type { AuthUserData } from '../../auth/user.js';

import type { AccountPrincipal, IdentityPrincipal } from "./handler/types.js";
import type { OAuthData } from "./hooks.js";
import { authenticateScheme, defaultScheme, type SchemeAuthentication } from "./schemes.js";
import { toWebRequest } from "./http.js";
import { getCurrentRequest } from "../requestContext.js";

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
{=# userFieldsFromClaims.isDefined =}
{=& userFieldsFromClaims.importStatement =}
{=/ userFieldsFromClaims.isDefined =}
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
    const authentication = await authenticateScheme(scheme, request);
    if (authentication === null) {
      continue;
    }
    const account = await accountOf(authentication);
    const user = await loadUser(account.authId, scheme, loginSchemeOf(authentication), account);
    if (user === null) {
      continue;
    }
    return { scheme, credentialId: account.credentialId, user };
  }
  return null;
}

// PRIVATE API
/**
 * Whose request this is, resolved to the ACCOUNT: Wasp's own credential
 * names it directly; a credential handler's identity is looked up, and
 * provisioned on first sight. What `runtime.authenticate` answers with.
 */
export async function authenticateAccount(scheme: AuthSchemeName, request: Request): Promise<AccountPrincipal | null> {
  const authentication = await authenticateScheme(scheme, request);
  return authentication === null ? null : accountOf(authentication);
}

async function accountOf(authentication: SchemeAuthentication): Promise<AccountPrincipal> {
  if (authentication.kind === 'account') {
    const { authId, credentialId, credentialIssuedAt, isCredentialFresh } = authentication.account;
    return { authId, credentialId, credentialIssuedAt, isCredentialFresh };
  }
  const { principal } = authentication;
  return {
    authId: await resolveSubject(authentication.scheme, principal.providerUserId, principal.claims, undefined, principal.providerName ?? 'default'),
    credentialId: principal.credentialId,
    credentialIssuedAt: principal.credentialIssuedAt ?? null,
    isCredentialFresh: principal.isCredentialFresh ?? false,
  };
}

function loginSchemeOf(authentication: SchemeAuthentication): string {
  return authentication.kind === 'account' ? authentication.loginScheme : authentication.scheme;
}

/** The user data Wasp exposes as `context.user`, for an account. */
async function loadUser(
  authId: string,
  scheme: AuthSchemeName,
  loginScheme: string,
  credential: Pick<AccountPrincipal, 'credentialIssuedAt' | 'isCredentialFresh'>,
): Promise<AuthUserData | null> {
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

  return createAuthUserData(user, scheme, loginScheme, {
    credentialIssuedAt: credential.credentialIssuedAt,
    isCredentialFresh: credential.isCredentialFresh,
  });
}

// The scheme's `userFieldsFromClaims` compute a provisioned user's own fields
// from the claims that scheme verified. Each scheme brings its own.
const userFieldsFromClaimsByScheme: Partial<Record<AuthSchemeName, unknown>> = {
  {=# schemes =}
  '{= schemeName =}': {=# userFieldsFromClaims.isDefined =}{= userFieldsFromClaims.importIdentifier =}{=/ userFieldsFromClaims.isDefined =}{=^ userFieldsFromClaims.isDefined =}undefined{=/ userFieldsFromClaims.isDefined =},
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
  providerUserId: string,
  claims: IdentityPrincipal['claims'],
  identity?: {
    data?: Record<string, unknown>;
    secrets?: Record<string, unknown>;
  },
  // The provider name to record under; a scheme that declared several
  // multiplexes them, everyone else records under `default`. The runtime
  // guards membership before we get here.
  providerName: string = 'default',
  // Present when the handler provisions through an OAuth provider.
  oauth?: OAuthData,
): Promise<string> {
  const req = getCurrentRequest();
  const identities = getIdentityStore(scheme, providerName);

  const existing = await identities.find(providerUserId);
  if (existing) {
    return existing.authId;
  }

  // A brand-new subject IS a signup, so the app's signup hooks fire here --
  // this choke point is what makes the app's veto cover every scheme. The
  // veto runs BEFORE the `userSignupFields` getters.
  await fireVetoableHook(() =>
    onBeforeSignupHook({
      req,
      providerId: makeHookProviderId(scheme, providerName, providerUserId),
    }),
  );

  // The scheme's `userFieldsFromClaims` compute the new user's own fields from
  // the claims the handler verified -- the only way a user entity with
  // required columns can be provisioned at all. Computed only for brand-new
  // subjects.
  const userFields = await computeSchemeUserFields(scheme, claims);

  let created;
  try {
    created = await identities.createIdentity(
      providerUserId,
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
      const raced = await identities.find(providerUserId);
      if (raced === null) {
        // The winner's row is gone again: a concurrent delete. Nothing sane
        // to return, so say so instead of handing back "no user".
        throw contractError('wasp-auth/identity-not-found', 'The identity vanished while it was being provisioned.');
      }
      return raced.authId;
    }
    throw e;
  }

  await onAfterSignupHook({
    req,
    providerId: makeHookProviderId(scheme, providerName, providerUserId),
    user: created,
    oauth,
  });

  return created.{= authFieldOnUserEntityName =}!.id;
}

function contractError(code: string, message: string): Error {
  const error = new Error(message) as Error & { code: string };
  error.code = code;
  return error;
}

function isUniqueConstraintViolation(e: unknown): boolean {
  return (
    typeof e === 'object' && e !== null && 'code' in e && (e as { code: unknown }).code === 'P2002'
  );
}

function makeHookProviderId(handlerName: string, providerName: string, providerUserId: string): ProviderId {
  return { handlerName, providerName, providerUserId };
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
  providerUserId: string,
  claims: IdentityPrincipal['claims'],
  identity?: {
    data?: Record<string, unknown>;
    secrets?: Record<string, unknown>;
  },
  providerName?: string,
  opts?: { oauth?: OAuthData },
): Promise<{ authId: string }> {
  const authId = await resolveSubject(scheme, providerUserId, claims, identity, providerName, opts?.oauth);
  return { authId };
}

// PRIVATE API
/**
 * Runs the scheme's manifest-level `userFieldsFromClaims` over verified claims,
 * producing the user entity's own fields. Shared by just-in-time provisioning
 * and the identity facet's `create` (when no field getter is passed).
 */
export async function computeSchemeUserFields(
  scheme: AuthSchemeName,
  claims: IdentityPrincipal['claims'],
): Promise<Record<string, unknown>> {
  return validateAndGetUserFields(
    { ...(claims ?? {}) },
    userFieldsFromClaimsByScheme[scheme] as any,
  );
}
