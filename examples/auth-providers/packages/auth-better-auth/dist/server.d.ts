import type { ServerAdapterFactory } from "@wasp.sh/auth-contract";
import { type BetterAuthOptions } from "better-auth";
/**
 * The type of the `setupFn` an app can reference from its manifest, following
 * the same convention as Wasp's `PrismaSetupFn`: it receives the adapter's
 * integration config (database adapter, secret, base URL and path, trusted
 * origins, table name overrides, bearer transport) and returns the Better
 * Auth options to use.
 *
 * The returned configuration is authoritative and has plain Better Auth
 * semantics: **nothing is enabled unless you enable it** -- `emailAndPassword`,
 * `socialProviders`, `databaseHooks`, `plugins`, email callbacks, exactly as
 * Better Auth's own documentation describes. Spread the received config to
 * keep the integration settings, then add yours.
 *
 * The adapter re-asserts its load-bearing settings after calling it (base
 * path, `modelName` overrides, the bearer plugin, the database adapter), so
 * those cannot be broken from here -- everything else is yours.
 */
export type BetterAuthSetupFn = (config: BetterAuthOptions) => BetterAuthOptions;
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
export declare const createServerAdapter: ServerAdapterFactory;
