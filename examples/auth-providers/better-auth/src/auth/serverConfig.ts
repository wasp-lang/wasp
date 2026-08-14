import type { BetterAuthConfigExtension } from "@wasp.sh/auth-better-auth/server";

/**
 * The escape hatch in action: this function receives the adapter's default
 * Better Auth configuration and may change anything the adapter does not
 * re-assert (base path, table names, the bearer plugin, the database).
 *
 * Everything Better Auth's options can express is available here -- its own
 * hook system (`databaseHooks`, shown below), `plugins`, email delivery
 * callbacks (`emailAndPassword.sendResetPassword`,
 * `emailVerification.sendVerificationEmail`), rate limiting, session tuning.
 *
 * Note the division of labor: these are *Better Auth's* hooks, running inside
 * Better Auth's flows. Wasp-level concerns stay in Wasp: populating the app's
 * own `User` row happens through the manifest's `userSignupFields`, fed by the
 * claims the adapter verified.
 */
export const extendBetterAuth: BetterAuthConfigExtension = (config) => ({
  ...config,
  databaseHooks: {
    user: {
      create: {
        after: async (user) => {
          console.log(`[better-auth hook] user created: ${user.email}`);
        },
      },
    },
    session: {
      create: {
        after: async (session) => {
          console.log(`[better-auth hook] session created: ${session.id}`);
        },
      },
    },
  },
});
