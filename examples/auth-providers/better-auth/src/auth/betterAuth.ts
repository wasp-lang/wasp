import { betterAuth } from "better-auth";
import { prismaAdapter } from "better-auth/adapters/prisma";
import { bearer } from "better-auth/plugins";
import { prisma } from "wasp/server";

/**
 * A stock Better Auth instance. Nothing here knows about Wasp.
 *
 * Two settings are load-bearing for this integration:
 *
 * - `bearer()` — Wasp sends the session token in an `Authorization: Bearer`
 *   header rather than a cookie, so the same client code works from a browser,
 *   a native app or a script. Better Auth supports that through this plugin.
 *
 * - `modelName` on every model — Better Auth's default table names (`user`,
 *   `session`, `account`) would collide with Wasp's own generated tables. Note
 *   these must be the *Prisma client property*, not the `@@map` name: the
 *   adapter does a raw `db[modelName]` lookup with no case transformation.
 */
export const auth = betterAuth({
  database: prismaAdapter(prisma, { provider: "sqlite" }),
  secret: process.env.BETTER_AUTH_SECRET,
  baseURL: process.env.WASP_SERVER_URL ?? "http://localhost:3001",
  basePath: "/better-auth",

  user: { modelName: "betterAuthUser" },
  session: { modelName: "betterAuthSession" },
  account: { modelName: "betterAuthAccount" },
  verification: { modelName: "betterAuthVerification" },

  emailAndPassword: {
    enabled: true,
    // This example is about the provider interface, not about email delivery.
    requireEmailVerification: false,
  },

  plugins: [bearer()],
});
