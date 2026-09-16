import { canManageSessions as canProviderManageSessions, canRevokeSessions as canProviderRevokeSessions } from './types.js';
import { computeProviderUserFields, provisionAuthUser } from '../session.js';
import { getIdentityStore } from '../identityStore.js';
import * as sessionStore from '../sessionStore.js';
import { findAuthWithUserBy } from '../utils.js';
import { fireVetoableHook, onAfterLoginHook, onAfterSignupHook, onBeforeLoginHook, onBeforeSignupHook, } from '../hookDispatch.js';
import { config, prisma } from '../../index.js';
import { env as validatedEnv } from '../../env.js';
import { emailSender } from '../../email/index.js';
import { createServerAdapter as createServerAdapter_0 } from '@wasp.sh/auth/server';
// PRIVATE API
export { canManageSessions, canRevokeSessions, } from './types.js';
/**
 * Errors the granted facets reject with carry a `code` rather than being a
 * class: adapter packages hold their own copy of the contract, and
 * `instanceof` does not survive package-copy boundaries.
 */
function contractError(code, message) {
    const error = new Error(message);
    error.code = code;
    return error;
}
function isUniqueConstraintViolation(e) {
    return (typeof e === 'object' && e !== null && 'code' in e && e.code === 'P2002');
}
/**
 * The namespace-membership guard, run BEFORE any store access: the identity
 * store itself resolves any namespace string, so this check (not the lookup)
 * is what makes acting on another provider's user unrepresentable through the
 * granted facets.
 */
function resolveOwnNamespace(spec, namespace) {
    const resolved = namespace ?? spec.providerId;
    if (!spec.identityNamespaces.includes(resolved)) {
        throw contractError('wasp-auth/undeclared-namespace', `Auth provider '${spec.providerId}' tried to use the identity namespace '${resolved}', which its manifest does not declare.`);
    }
    return resolved;
}
/** The contract-shaped identity facet for one of the provider's namespaces. */
function makeIdentitiesFacet(spec, namespace) {
    const store = getIdentityStore(namespace);
    return {
        find: (subjectId) => store.find(subjectId),
        provision: (subjectId, identity) => provisionAuthUser(spec.providerId, subjectId, identity?.claims, {
            data: identity?.data,
            secrets: identity?.secrets,
        }, namespace),
        create: async (subjectId, identity, getUserFields, opts) => {
            // The app's signup veto fires FIRST -- at this Wasp-owned choke point no
            // provider can forget it -- and only then do any user-supplied field
            // getters run (that ordering is why `getUserFields` is a lazy callback).
            if (opts?.skipHooks !== true) {
                await fireVetoableHook(() => onBeforeSignupHook({
                    req: opts?.req,
                    providerId: makeHookProviderId(namespace, subjectId),
                }));
            }
            const userFields = getUserFields !== undefined
                ? await getUserFields()
                : await computeProviderUserFields(spec.providerId, identity?.claims);
            let created;
            try {
                created = await store.createIdentity(subjectId, identity, userFields);
            }
            catch (e) {
                if (isUniqueConstraintViolation(e)) {
                    throw contractError('wasp-auth/duplicate-identity', `An identity for this subject already exists in namespace '${namespace}'.`);
                }
                throw e;
            }
            if (opts?.skipHooks !== true) {
                await onAfterSignupHook({
                    req: opts?.req,
                    providerId: makeHookProviderId(namespace, subjectId),
                    user: created,
                    oauth: opts?.hookContext,
                });
            }
            return { authId: created.auth.id };
        },
        updateData: (subjectId, updates) => store.updateData(subjectId, updates),
        getSecrets: (subjectId) => store.getSecrets(subjectId),
        setSecrets: (subjectId, secrets) => store.setSecrets(subjectId, secrets),
        deleteUser: (subjectId) => store.deleteUser(subjectId),
    };
}
/**
 * The `wasp-sessions` grant. Minting is subject-bound (the namespace guard
 * plus the identity lookup), and the session records THIS provider's id --
 * the inputs `authRequired: [...]` enforcement trusts. `revokeAllForSubject`
 * is deliberately the raw store call, NOT the dual-sign-out loop
 * (`invalidateAllSessionsForAuthId` re-enters `provider.revokeSession`, so an
 * adapter calling it from its own revocation path would recurse).
 */
function makeSessionsFacet(spec) {
    const resolveSubjectAuthId = async (subject) => {
        const namespace = resolveOwnNamespace(spec, subject.namespace);
        const identity = await getIdentityStore(namespace).find(subject.subjectId);
        if (identity === null) {
            throw contractError('wasp-auth/identity-not-found', `No identity for the subject in namespace '${namespace}'. Provision it before minting or revoking sessions.`);
        }
        return identity.authId;
    };
    return {
        issue: async (subject, opts) => {
            const authId = await resolveSubjectAuthId(subject);
            // The app's login hooks fire around every mint at this Wasp-owned choke
            // point (veto by throwing), whichever provider is minting. `skipHooks`
            // exists for flows that already fired them at a more informative moment
            // (wasp-auth's OAuth callback holds the tokens; the redeem route does
            // not).
            const fireHooks = opts?.skipHooks !== true;
            const hookProviderId = makeHookProviderId(resolveOwnNamespace(spec, subject.namespace), subject.subjectId);
            let hookUser = undefined;
            if (fireHooks) {
                const auth = await findAuthWithUserBy({ id: authId });
                if (auth === null) {
                    throw contractError('wasp-auth/identity-not-found', 'The subject resolves to an auth entity with no user.');
                }
                hookUser = auth.user;
                await fireVetoableHook(() => onBeforeLoginHook({
                    req: opts?.req,
                    providerId: hookProviderId,
                    user: auth.user,
                }));
            }
            const session = await sessionStore.createSession(authId, {
                providerId: spec.providerId,
                providerSessionId: opts?.providerSessionId,
            });
            if (fireHooks) {
                await onAfterLoginHook({
                    req: opts?.req,
                    providerId: hookProviderId,
                    user: hookUser,
                    oauth: opts?.hookContext,
                });
            }
            return { sessionId: session.id };
        },
        revoke: (sessionId) => sessionStore.revokeSession(sessionId),
        revokeAllForSubject: async (subject) => {
            const authId = await resolveSubjectAuthId(subject);
            await sessionStore.revokeAllSessions(authId);
        },
    };
}
// The hook payloads speak `ProviderId` ({ providerName, providerUserId });
// external namespaces are not in the generated `ProviderName` union, so the
// cast widens it -- the values are plain strings either way.
function makeHookProviderId(namespace, subjectId) {
    return { providerName: namespace, providerUserId: subjectId };
}
/**
 * The `email-send` grant: the app's configured email sender, sender identity
 * included. SMTP credentials never reach the adapter -- only the send
 * capability does.
 */
const waspEmailFacet = (() => {
    // Aeson encodes an absent name as null; the contract speaks `name?: string`.
    const configured = { "email": "auth@example.com", "name": "Wasp Auth Lib" };
    const defaultFrom = configured === undefined
        ? undefined
        : { email: configured.email, ...(configured.name ? { name: configured.name } : {}) };
    return {
        defaultFrom,
        send: async (email) => {
            const from = email.from ?? defaultFrom;
            if (from === undefined) {
                throw new Error('Sending an email through the auth provider runtime requires a `from` field, because the app declares no emailSender.defaultFrom.');
            }
            await emailSender.send({
                from,
                to: email.to,
                subject: email.subject,
                text: email.text,
                html: email.html,
            });
        },
    };
})();
function makeAdapterRuntime(spec) {
    return {
        db: prisma,
        dbProvider: 'sqlite',
        // Exactly the vars the manifest declared -- read from the VALIDATED env,
        // so `devDefault`s apply -- and framework secrets (JWT_SECRET) stay
        // unreachable (declaring a framework-owned name is a compile error).
        env: Object.fromEntries(spec.serverEnvVarNames.map((name) => [
            name,
            validatedEnv[name],
        ])),
        serverUrl: config.serverUrl,
        clientUrl: config.frontendUrl,
        isDevelopment: config.isDevelopment,
        identities: makeIdentitiesFacet(spec, spec.providerId),
        // Granted facets: wired only when the manifest requested them, so an
        // undeclared access fails loudly at first use rather than working by
        // accident.
        ...(spec.uses.includes('wasp-sessions') ? { sessions: makeSessionsFacet(spec) } : {}),
        ...(spec.uses.includes('email-send') ? { email: waspEmailFacet } : {}),
        ...(spec.uses.includes('identity-namespaces')
            ? {
                identityNamespaces: (namespace) => makeIdentitiesFacet(spec, resolveOwnNamespace(spec, namespace)),
            }
            : {}),
    };
}
/**
 * The adapter package's server factory for 'external:wasp-auth', called with
 * everything it may know about the app.
 */
const serverAdapter_0 = await Promise.resolve(createServerAdapter_0(
// The cast narrows the built runtime to the grants the factory's type
// declares; the generator wired exactly the manifest's `uses`, and the
// boot assert keeps manifest and adapter honest.
makeAdapterRuntime({
    providerId: 'external:wasp-auth',
    serverEnvVarNames: ['WASP_AUTH_TOKENS_SECRET', 'WASP_AUTH_GOOGLE_CLIENT_ID', 'WASP_AUTH_GOOGLE_CLIENT_SECRET'],
    uses: ['wasp-sessions', 'identity-namespaces', 'email-send'],
    identityNamespaces: ['external:wasp-auth', 'external:wasp-auth/username', 'external:wasp-auth/email', 'external:wasp-auth/google'],
}), { "methods": { "usernameAndPassword": {}, "email": { "emailVerificationPath": "/email-verified", "passwordResetPath": "/password-reset" }, "google": {} }, "oauthCallbackPath": "/oauth/callback" }, {
    // The user's setup function for the adapter's underlying library; the
    // adapter calls it with its integration config and uses the result.
    setupFn: undefined,
}));
// PRIVATE API
/**
 * The app's auth providers, keyed by provider id, in `main.wasp.ts`
 * declaration order.
 *
 * Everything else in Wasp depends on the `AuthProvider` interface rather than
 * on concrete implementations. Every provider a session can name is here, so
 * looking up a session's minting provider always succeeds.
 */
export const authProviders = {
    'external:wasp-auth': serverAdapter_0.provider,
};
// PRIVATE API
/**
 * The external providers a credential can be exchanged with (`POST
 * /auth/login/:providerId`). Deliberately excludes 'wasp': Wasp's own auth
 * mints sessions through its own routes, and exchanging a Wasp credential for
 * a Wasp session would be a loop.
 */
export const externalAuthProviders = {
    'external:wasp-auth': authProviders['external:wasp-auth'],
};
// PRIVATE API
export function getAuthProvider(providerId) {
    return authProviders[providerId];
}
// PRIVATE API
/**
 * Node handlers for the routes external providers brought with them, keyed by
 * provider id. The server mounts each at the basePath its manifest declared.
 */
export const authProviderRouteHandlers = {
    'external:wasp-auth': serverAdapter_0.routeHandler,
};
/**
 * Each manifest in `main.wasp.ts` made compile-time claims about its provider
 * (its id, its capabilities), and code was generated from them. Checking the
 * claims against the adapter objects at boot turns a wrong manifest into a
 * loud startup failure instead of a subtly broken app.
 */
function assertProvidersMatchManifests() {
    const manifests = [
        { providerId: 'external:wasp-auth', capabilities: [], uses: ['wasp-sessions', 'identity-namespaces', 'email-send'] },
    ];
    const knownRuntimeGrants = ['wasp-sessions', 'email-send', 'identity-namespaces'];
    const errors = [];
    for (const manifest of manifests) {
        const provider = getAuthProvider(manifest.providerId);
        if (provider === undefined) {
            continue;
        }
        // Both rules below are compile-time errors too (mapper + Haskell
        // validator); asserting them here as well means no generated-code path can
        // quietly outlive a validation gap.
        if (!manifest.providerId.startsWith('external:')) {
            errors.push(`the manifest declares id '${manifest.providerId}', which does not start with 'external:' -- ` +
                `the unprefixed namespace is reserved for Wasp's own auth methods`);
        }
        if (manifest.capabilities.includes('cookie-transport') &&
            !manifest.capabilities.includes('session-revocation')) {
            errors.push(`the manifest for '${manifest.providerId}' declares 'cookie-transport' without 'session-revocation' -- ` +
                `a cookie-borne credential Wasp cannot revoke server-side would make logout() a lie`);
        }
        for (const grant of manifest.uses) {
            if (!knownRuntimeGrants.includes(grant)) {
                errors.push(`the manifest for '${manifest.providerId}' requests the unknown runtime grant '${grant}' -- ` +
                    `the generator could not have wired it`);
            }
        }
        if (provider.id !== manifest.providerId) {
            errors.push(`the manifest declares id '${manifest.providerId}', but the adapter's id is '${provider.id}' -- ` +
                `identities are recorded under the provider id, so the two must match`);
        }
        if (manifest.capabilities.includes('issue-sessions') &&
            !canProviderManageSessions(provider)) {
            errors.push(`the manifest for '${manifest.providerId}' declares the 'issue-sessions' capability, but the adapter does not implement the full ` +
                `issueSession/revokeSession/revokeAllSessions set Wasp requires for session management`);
        }
        if (manifest.capabilities.includes('session-revocation') &&
            !canProviderRevokeSessions(provider)) {
            errors.push(`the manifest for '${manifest.providerId}' declares the 'session-revocation' capability, but the adapter does not implement revokeSession`);
        }
    }
    if (errors.length > 0) {
        throw new Error('Auth provider adapters do not match their manifests:\n' +
            errors.map((error) => `  - ${error}`).join('\n'));
    }
}
assertProvidersMatchManifests();
//# sourceMappingURL=index.js.map