import { type User, type Auth } from '../../entities/index.js';
import { type PossibleProviderData, type PossibleProviderSecrets, type ProviderName } from '../../auth/providerData.js';
import { type PossibleUserFields } from '../../auth/providers/types.js';
/**
 * Wasp's identity store: THE way to read and write auth identities, for Wasp's
 * own auth and for user-made providers alike -- Wasp's auth flows go through the
 * exact same facet a hand-written provider gets, with no privileged access.
 *
 * A facet is scoped to one `providerName` (Wasp's own auth multiplexes several:
 * `email`, `username`, the OAuth providers; an external provider has exactly
 * one, its manifest id). The facet's three data channels mirror the identity's
 * three columns:
 *
 * - `claims`  -- what the provider asserted at login; written at creation,
 *   read-only afterwards, so its provenance can be trusted.
 * - `data`    -- non-secret working state; partial updates via `updateData`.
 * - `secrets` -- secret material, in the column the Prisma client omits by
 *   default. Read and written ONLY through `getSecrets`/`setSecrets`, and
 *   stored as given: hashing is the caller's explicit job (see `hashPassword`
 *   in `wasp/server/auth`), never a side effect of storage.
 */
export type Identity<Data extends object> = {
    providerName: string;
    providerUserId: string;
    authId: string;
    /** Non-secret working state (the `providerData` column, parsed). */
    data: Data;
    /** Provider-asserted, Wasp-recorded profile data (the `providerClaims` column, parsed). */
    claims: Record<string, unknown>;
};
export type CreateUserResult = User & {
    auth: Auth | null;
};
export type IdentityStore<Data extends object, Secrets extends object> = {
    /** Reads the identity (never its secrets). */
    find(providerUserId: string): Promise<Identity<Data> | null>;
    /**
     * Creates the user with its auth identity in one atomic write. A duplicate
     * identity surfaces as Prisma's unique-constraint error (P2002), same as any
     * other conflicting write -- see `rethrowPossibleAuthError`.
     */
    createIdentity(providerUserId: string, identity?: {
        claims?: Record<string, unknown>;
        data?: Data;
        secrets?: Secrets;
    }, userFields?: PossibleUserFields): Promise<CreateUserResult>;
    /**
     * Idempotent create: returns the existing identity's `authId` when the
     * subject is already known, creates it otherwise. Two concurrent calls for
     * the same brand-new subject are settled by the unique constraint -- the
     * loser re-reads and returns the winner's row.
     */
    provision(providerUserId: string, identity?: {
        claims?: Record<string, unknown>;
        data?: Data;
        secrets?: Secrets;
    }, userFields?: PossibleUserFields): Promise<{
        authId: string;
    } | null>;
    /**
     * Reads the identity's secret material -- the single opt-in into the column
     * the Prisma client omits by default. Keep the result on the server.
     */
    getSecrets(providerUserId: string): Promise<Secrets | null>;
    /** Replaces the identity's secret material. Expects it **already hashed**. */
    setSecrets(providerUserId: string, secrets: Secrets): Promise<void>;
    /** Merges the given updates into the identity's non-secret data. */
    updateData(providerUserId: string, updates: Partial<Data>): Promise<void>;
    /**
     * Deletes the identity's whole user (cascading to its auth data and
     * sessions). Returns whether anything was deleted.
     */
    deleteUser(providerUserId: string): Promise<boolean>;
};
/**
 * The facet for one of Wasp's own auth methods, typed with its data shapes.
 */
export declare function getIdentityStore<PN extends ProviderName>(providerName: PN): IdentityStore<PossibleProviderData[PN], PossibleProviderSecrets[PN]>;
/**
 * The facet for a user-made provider (e.g. an `external:*` id): same powers
 * Wasp's own auth uses, with untyped data shapes -- the provider owns them.
 */
export declare function getIdentityStore(providerName: string): IdentityStore<Record<string, unknown>, Record<string, unknown>>;
//# sourceMappingURL=identityStore.d.ts.map