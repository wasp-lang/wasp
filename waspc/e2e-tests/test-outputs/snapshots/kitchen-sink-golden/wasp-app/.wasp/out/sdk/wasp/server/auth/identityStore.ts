import { prisma } from '../index.js'
import {
  type User,
  type Auth,
} from '../../entities/index.js'
import { type PossibleUserFields } from '../../auth/providers/types.js'

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

// PUBLIC API
export type Identity<Data extends object> = {
  providerName: string;
  providerUserId: string;
  authId: string;
  /** Non-secret working state (the `providerData` column, parsed). */
  data: Data;
  /** Provider-asserted, Wasp-recorded profile data (the `providerClaims` column, parsed). */
  claims: Record<string, unknown>;
}

// PUBLIC API
export type CreateUserResult = User & {
  auth: Auth | null
}

// PUBLIC API
export type IdentityStore<Data extends object, Secrets extends object> = {
  /** Reads the identity (never its secrets). */
  find(providerUserId: string): Promise<Identity<Data> | null>;

  /**
   * Creates the user with its auth identity in one atomic write. A duplicate
   * identity surfaces as Prisma's unique-constraint error (P2002), same as any
   * other conflicting write -- see `rethrowPossibleAuthError`.
   */
  createIdentity(
    providerUserId: string,
    identity?: {
      claims?: Record<string, unknown>;
      data?: Data;
      secrets?: Secrets;
    },
    userFields?: PossibleUserFields,
  ): Promise<CreateUserResult>;

  /**
   * Idempotent create: returns the existing identity's `authId` when the
   * subject is already known, creates it otherwise. Two concurrent calls for
   * the same brand-new subject are settled by the unique constraint -- the
   * loser re-reads and returns the winner's row.
   */
  provision(
    providerUserId: string,
    identity?: {
      claims?: Record<string, unknown>;
      data?: Data;
      secrets?: Secrets;
    },
    userFields?: PossibleUserFields,
  ): Promise<{ authId: string } | null>;

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
}

// PUBLIC API
/**
 * The facet for one identity namespace (a provider id, or one of a
 * provider's namespaces like `wasp:email`). Data shapes are the provider's
 * own; normalizing the subject id (lower-casing an email, say) is the
 * provider's job before it calls in.
 */
export function getIdentityStore(
  providerName: string,
): IdentityStore<Record<string, unknown>, Record<string, unknown>> {
  const whereIdentity = (providerUserId: string) => ({
    providerName_providerUserId: {
      providerName,
      providerUserId,
    },
  });

  return {
    async find(providerUserId) {
      const identity = await prisma.authIdentity.findUnique({
        where: whereIdentity(providerUserId),
      });
      if (identity === null) {
        return null;
      }
      return {
        providerName: identity.providerName,
        providerUserId: identity.providerUserId,
        authId: identity.authId,
        data: JSON.parse(identity.providerData),
        claims: JSON.parse(identity.providerClaims),
      };
    },

    async createIdentity(providerUserId, identity, userFields) {
      return prisma.user.create({
        data: {
          // Using any here to prevent type errors when userFields are not
          // defined. We want Prisma to throw an error in that case.
          ...(userFields ?? {} as any),
          auth: {
            create: {
              identities: {
                create: {
                  providerName,
                  providerUserId: providerUserId,
                  providerClaims: JSON.stringify(identity?.claims ?? {}),
                  providerData: JSON.stringify(identity?.data ?? {}),
                  providerSecrets: JSON.stringify(identity?.secrets ?? {}),
                },
              },
            }
          },
        },
        // We need to include the Auth entity here because we need `authId`
        // to be able to create a session.
        include: {
          auth: true,
        },
      })
    },

    async provision(providerUserId, identity, userFields) {
      const existing = await this.find(providerUserId);
      if (existing !== null) {
        return { authId: existing.authId };
      }
      try {
        const created = await this.createIdentity(providerUserId, identity, userFields);
        return { authId: created.auth!.id };
      } catch (e: unknown) {
        // Another request provisioned the same subject between our read and
        // our write. Its row is the winner; re-read rather than failing.
        if (isUniqueConstraintViolation(e)) {
          const raced = await this.find(providerUserId);
          return raced === null ? null : { authId: raced.authId };
        }
        throw e;
      }
    },

    async getSecrets(providerUserId) {
      const identity = await prisma.authIdentity.findUnique({
        where: whereIdentity(providerUserId),
        omit: { providerSecrets: false },
      });
      return identity === null ? null : JSON.parse(identity.providerSecrets);
    },

    async setSecrets(providerUserId, secrets) {
      await prisma.authIdentity.update({
        where: whereIdentity(providerUserId),
        data: { providerSecrets: JSON.stringify(secrets) },
      });
    },

    async updateData(providerUserId, updates) {
      const identity = await prisma.authIdentity.findUnique({
        where: whereIdentity(providerUserId),
        select: { providerData: true },
      });
      if (identity === null) {
        throw new Error('Auth identity not found.');
      }
      const newData = { ...JSON.parse(identity.providerData), ...updates };
      await prisma.authIdentity.update({
        where: whereIdentity(providerUserId),
        data: { providerData: JSON.stringify(newData) },
      });
    },

    async deleteUser(providerUserId) {
      const { count } = await prisma.user.deleteMany({
        where: {
          auth: {
            identities: {
              some: {
                providerName,
                providerUserId: providerUserId,
              },
            },
          },
        },
      });
      return count > 0;
    },
  };
}

function isUniqueConstraintViolation(e: unknown): boolean {
  return (
    typeof e === 'object' && e !== null && 'code' in e && (e as { code: unknown }).code === 'P2002'
  );
}
