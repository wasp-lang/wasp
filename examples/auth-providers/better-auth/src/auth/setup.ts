import type { BetterAuthSetupFn } from "@wasp.sh/auth-better-auth/server";

/**
 * The setup function in action -- the `prismaSetupFn` convention applied to
 * auth. It receives the adapter's integration config (database, secret, base
 * path, table names, bearer transport) and returns the Better Auth options to
 * use. From here Better Auth is yours in full, with its own semantics:
 * **nothing is enabled unless this function enables it**, which is why
 * `emailAndPassword` is switched on explicitly below.
 *
 * Everything Better Auth's options can express is available -- its own hook
 * system (`databaseHooks`, shown below), `plugins`, email delivery callbacks
 * (`emailAndPassword.sendResetPassword`,
 * `emailVerification.sendVerificationEmail`), rate limiting, session tuning.
 *
 * Note the division of labor: these are *Better Auth's* hooks, running inside
 * Better Auth's flows. Wasp-level concerns stay in Wasp: populating the app's
 * own `User` row happens through the manifest's `userSignupFields`, fed by the
 * claims the adapter verified.
 */
export const setupBetterAuth: BetterAuthSetupFn = (config) => ({
  ...config,
  // Plain Better Auth semantics: enable exactly what the app wants.
  emailAndPassword: { enabled: true, requireEmailVerification: false },
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
