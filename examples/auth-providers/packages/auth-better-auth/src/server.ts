import type {
  ServerAdapterFactory,
  VerifiedSession,
} from "@wasp.sh/auth-contract";
import { betterAuth } from "better-auth";
import { prismaAdapter } from "better-auth/adapters/prisma";
import { toNodeHandler } from "better-auth/node";
import { bearer } from "better-auth/plugins";

/** The serializable options the spec helper captured in `main.wasp.ts`. */
export type BetterAuthAdapterOptions = {
  emailAndPassword?: boolean;
};

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
export const createServerAdapter: ServerAdapterFactory<
  BetterAuthAdapterOptions
> = (runtime, options) => {
  const auth = betterAuth({
    // The app's own PrismaClient, handed over by Wasp. `runtime.db` is typed
    // `unknown` because the client's type is generated per app; Better Auth's
    // adapter only needs its dynamic model delegates.
    database: prismaAdapter(runtime.db as never, {
      provider: runtime.dbProvider as "sqlite",
    }),
    secret: runtime.env.BETTER_AUTH_SECRET,
    baseURL: runtime.serverUrl,
    basePath: "/better-auth",
    trustedOrigins: [runtime.clientUrl],

    user: { modelName: "betterAuthUser" },
    session: { modelName: "betterAuthSession" },
    account: { modelName: "betterAuthAccount" },
    verification: { modelName: "betterAuthVerification" },

    emailAndPassword: {
      enabled: options?.emailAndPassword ?? true,
      // Email delivery is not wired through the adapter (yet), so requiring
      // verification would lock every user out.
      requireEmailVerification: false,
    },

    plugins: [bearer()],
  });

  return {
    provider: {
      /**
       * Becomes `AuthIdentity.providerName` for every user provisioned through
       * this adapter, so it must stay stable across deploys. Changing it
       * orphans users.
       */
      id: "better-auth",

      /**
       * Wasp hands every adapter a standard web `Request` -- built from the
       * HTTP request, or synthesized with just an `Authorization` header for
       * websocket auth. Better Auth consumes its headers directly either way.
       */
      async authenticate(request: Request): Promise<VerifiedSession | null> {
        const session = await auth.api.getSession({
          headers: request.headers,
        });

        if (!session) {
          return null;
        }

        return {
          sessionId: session.session.id,
          subjectId: session.user.id,
          // Verified profile data Wasp records when it provisions the local
          // user.
          claims: {
            email: session.user.email,
            name: session.user.name,
          },
        };
      },

      /**
       * Revokes by deleting the session row through the app's Prisma client.
       *
       * Better Auth's own HTTP API cannot do this: `revokeSession` takes a
       * session *token* and needs an authenticated session in its headers to
       * find one, and at logout time Wasp holds only the session *id*. The
       * adapter owns the table it configured above (`betterAuthSession`), so
       * revocation by id through the database is the honest implementation.
       */
      async revokeSession(sessionId: string): Promise<void> {
        const db = runtime.db as {
          betterAuthSession: {
            deleteMany(args: { where: { id: string } }): Promise<unknown>;
          };
        };
        await db.betterAuthSession.deleteMany({ where: { id: sessionId } });
      },
    },

    /**
     * Better Auth's own HTTP surface (sign-up, sign-in, sign-out, OAuth
     * callbacks). Wasp mounts it at the manifest's `basePath` with the JSON
     * body parser stripped (`rawBody: true`) -- `toNodeHandler` reads the raw
     * request stream, and an already-consumed stream hangs every request.
     */
    routeHandler: toNodeHandler(auth),
  };
};
