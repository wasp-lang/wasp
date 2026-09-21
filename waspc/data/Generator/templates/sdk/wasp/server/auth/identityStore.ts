{{={= =}=}}
import { prisma } from '../index.js'
import {
  type {= userEntityUpper =},
  type {= authEntityUpper =},
} from '../../entities/index.js'
import { type PossibleUserFields } from '../../auth/providers/types.js'

/**
 * Wasp's identity store: THE way to read and write auth identities, for Wasp's
 * own auth and for user-made providers alike -- Wasp's auth flows go through the
 * exact same facet a hand-written provider gets, with no privileged access.
 *
 * A facet is scoped to one handler's one provider name: the `handlerName` and
 * `providerName` columns of the identity (Wasp's own auth has several provider
 * names: `email`, `username`, the OAuth providers; a handler with one kind of
 * login has just `default`). Together with `providerUserId` they are the
 * identity's primary key. The facet's three data channels mirror the
 * identity's three other columns:
 *
 * - `claims`  -- what the provider asserted at login; written at creation,
 *   read-only afterwards, so its provenance can be trusted.
 * - `data`    -- non-secret working state; merged into via `updateData`.
 * - `secrets` -- secret material, in the column the Prisma client omits by
 *   default. Read and written ONLY through `getSecrets`/`updateSecrets`, and
 *   stored as given: hashing is the caller's explicit job (see `hashPassword`
 *   in `wasp/server/auth`), never a side effect of storage.
 */

// PUBLIC API
export type Identity<Data extends object> = {
  handlerName: string;
  providerName: string;
  providerUserId: string;
  authId: string;
  /** Non-secret working state (the `providerData` column, parsed). */
  data: Data;
  /** Provider-asserted, Wasp-recorded profile data (the `providerClaims` column, parsed). */
  claims: Record<string, unknown>;
}

// PUBLIC API
/** Some of `T`'s keys, each with a new value or `null` to remove it. */
export type MergePatch<T extends object> = { [Key in keyof T]?: T[Key] | null }

// PUBLIC API
export type CreateUserResult = {= userEntityUpper =} & {
  {= authFieldOnUserEntityName =}: {= authEntityUpper =} | null
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

  /**
   * Merges the given updates into the identity's secret material, exactly
   * like `updateData`. Expects the values **already hashed**.
   */
  updateSecrets(providerUserId: string, updates: MergePatch<Secrets>): Promise<void>;

  /**
   * Merges the given updates into the identity's non-secret data. A key set
   * to `null` is removed; every key not named is left alone. Atomic: two
   * concurrent updates cannot lose one another.
   */
  updateData(providerUserId: string, updates: MergePatch<Data>): Promise<void>;

  /**
   * Deletes the identity, and with it the whole user (cascading to its auth
   * data and sessions) ONLY when it was the account's last identity: an
   * account nobody can log into is not kept. Returns whether there was an
   * identity to delete.
   */
  deleteIdentity(providerUserId: string): Promise<boolean>;

  /**
   * Account linking: attaches a new identity to an EXISTING Auth entity,
   * creating no user. A taken identity surfaces as Prisma's unique-constraint
   * error (P2002).
   */
  linkIdentity(
    providerUserId: string,
    identity: {
      claims?: Record<string, unknown>;
      data?: Data;
      secrets?: Secrets;
    },
    authId: string,
  ): Promise<void>;

  /**
   * Detaches the identity from the Auth entity, unless it is that entity's
   * last one. Settled in one transaction, so two concurrent unlinks cannot
   * leave an account with no way in.
   */
  unlinkIdentity(
    providerUserId: string,
    authId: string,
  ): Promise<'unlinked' | 'not-found' | 'last-identity'>;
}

// PUBLIC API
/**
 * The facet for one provider name of one auth handler (`wasp` / `email`).
 * Data shapes are the handler's own; normalizing the provider user id
 * (lower-casing an email, say) is the handler's job before it calls in.
 */
export function getIdentityStore(
  handlerName: string,
  providerName: string,
): IdentityStore<Record<string, unknown>, Record<string, unknown>> {
  const whereIdentity = (providerUserId: string) => ({
    handlerName_providerName_providerUserId: {
      handlerName,
      providerName,
      providerUserId,
    },
  });

  // Read and write in one transaction, so two concurrent merges into the same
  // identity cannot lose one another. A `null` value removes its key.
  const mergeIntoColumn = (
    providerUserId: string,
    column: 'providerData' | 'providerSecrets',
    updates: Record<string, unknown>,
  ) =>
    prisma.$transaction(async (tx) => {
      const identity = await tx.{= authIdentityEntityLower =}.findUnique({
        where: whereIdentity(providerUserId),
        omit: { providerSecrets: false },
      });
      if (identity === null) {
        throw new Error('Auth identity not found.');
      }
      const merged = { ...JSON.parse(identity[column]), ...updates };
      for (const [key, value] of Object.entries(updates)) {
        if (value === null) {
          delete merged[key];
        }
      }
      await tx.{= authIdentityEntityLower =}.update({
        where: whereIdentity(providerUserId),
        data: { [column]: JSON.stringify(merged) },
      });
    });

  return {
    async find(providerUserId) {
      const identity = await prisma.{= authIdentityEntityLower =}.findUnique({
        where: whereIdentity(providerUserId),
      });
      if (identity === null) {
        return null;
      }
      return {
        handlerName: identity.handlerName,
        providerName: identity.providerName,
        providerUserId: identity.providerUserId,
        authId: identity.authId,
        data: JSON.parse(identity.providerData),
        claims: JSON.parse(identity.providerClaims),
      };
    },

    async createIdentity(providerUserId, identity, userFields) {
      return prisma.{= userEntityLower =}.create({
        data: {
          // Using any here to prevent type errors when userFields are not
          // defined. We want Prisma to throw an error in that case.
          ...(userFields ?? {} as any),
          {= authFieldOnUserEntityName =}: {
            create: {
              {= identitiesFieldOnAuthEntityName =}: {
                create: {
                  // Always explicit: the column's default exists only to
                  // backfill rows written before the column did.
                  handlerName,
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
          {= authFieldOnUserEntityName =}: true,
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
        return { authId: created.{= authFieldOnUserEntityName =}!.id };
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
      const identity = await prisma.{= authIdentityEntityLower =}.findUnique({
        where: whereIdentity(providerUserId),
        omit: { providerSecrets: false },
      });
      return identity === null ? null : JSON.parse(identity.providerSecrets);
    },

    async updateSecrets(providerUserId, updates) {
      await mergeIntoColumn(providerUserId, 'providerSecrets', updates);
    },

    async updateData(providerUserId, updates) {
      await mergeIntoColumn(providerUserId, 'providerData', updates);
    },

    async linkIdentity(providerUserId, identity, authId) {
      await prisma.{= authIdentityEntityLower =}.create({
        data: {
          handlerName,
          providerName,
          providerUserId,
          providerClaims: JSON.stringify(identity.claims ?? {}),
          providerData: JSON.stringify(identity.data ?? {}),
          providerSecrets: JSON.stringify(identity.secrets ?? {}),
          authId,
        },
      });
    },

    async unlinkIdentity(providerUserId, authId) {
      return prisma.$transaction(async (tx) => {
        const identities = await tx.{= authIdentityEntityLower =}.findMany({
          where: { authId },
          select: { handlerName: true, providerName: true, providerUserId: true },
        });
        const holdsIdentity = identities.some(
          (identity) =>
            identity.handlerName === handlerName &&
            identity.providerName === providerName &&
            identity.providerUserId === providerUserId,
        );
        if (!holdsIdentity) {
          return 'not-found' as const;
        }
        if (identities.length === 1) {
          return 'last-identity' as const;
        }
        await tx.{= authIdentityEntityLower =}.delete({ where: whereIdentity(providerUserId) });
        return 'unlinked' as const;
      });
    },

    async deleteIdentity(providerUserId) {
      return prisma.$transaction(async (tx) => {
        const identity = await tx.{= authIdentityEntityLower =}.findUnique({
          where: whereIdentity(providerUserId),
          select: { authId: true },
        });
        if (identity === null) {
          return false;
        }
        const identitiesOnAccount = await tx.{= authIdentityEntityLower =}.count({
          where: { authId: identity.authId },
        });
        if (identitiesOnAccount > 1) {
          await tx.{= authIdentityEntityLower =}.delete({ where: whereIdentity(providerUserId) });
          return true;
        }
        // The last identity: nobody could log into what remains, so the user
        // goes too, cascading to its auth data and sessions.
        await tx.{= userEntityLower =}.deleteMany({
          where: { {= authFieldOnUserEntityName =}: { id: identity.authId } },
        });
        return true;
      });
    },
  };
}

function isUniqueConstraintViolation(e: unknown): boolean {
  return (
    typeof e === 'object' && e !== null && 'code' in e && (e as { code: unknown }).code === 'P2002'
  );
}
