import type { ServerAdapterFactory } from "@wasp.sh/auth-contract";
import { type BetterAuthOptions } from "better-auth";
/** The serializable options the spec helper captured in `main.wasp.ts`. */
export type BetterAuthAdapterOptions = {
    emailAndPassword?: boolean;
};
/**
 * The type of the `extendServerConfig` function an app can reference from its
 * manifest: it receives the adapter's default Better Auth configuration and
 * returns the configuration to use. This is the escape hatch to Better Auth's
 * full surface -- `databaseHooks`, `plugins`, `emailAndPassword.sendResetPassword`,
 * `emailVerification`, rate limiting, anything its options carry.
 *
 * The adapter re-asserts its load-bearing settings after applying it (base
 * path, `modelName` overrides, the bearer plugin, the database adapter), so
 * those cannot be broken from here -- everything else is yours.
 */
export type BetterAuthConfigExtension = (config: BetterAuthOptions) => BetterAuthOptions;
/**
 * Better Auth, expressed as a Wasp `AuthProvider`.
 *
 * One factory builds both the Better Auth instance and the provider that
 * verifies against it, so they are guaranteed to share one configuration --
 * the `ServerAdapter` shape exists to make the alternative unrepresentable.
 *
 * Two settings on the instance are load-bearing for this integration:
 *
 * - `bearer()` -- Wasp sends the session token in an `Authorization: Bearer`
 *   header rather than a cookie, so the same client code works from a browser,
 *   a native app or a script. Better Auth supports that through this plugin.
 *
 * - `modelName` on every model -- Better Auth's default table names (`user`,
 *   `session`, `account`) would collide with Wasp's own generated tables. Note
 *   these must be the *Prisma client property*, not the `@@map` name: the
 *   adapter does a raw `db[modelName]` lookup with no case transformation.
 */
export declare const createServerAdapter: ServerAdapterFactory<BetterAuthAdapterOptions>;
