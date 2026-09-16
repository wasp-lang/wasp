/**
 * Env var names the framework owns and no auth provider manifest may declare.
 *
 * Narrowed adapter runtimes receive exactly the vars their manifest declared,
 * so without this list an adapter could declare `DATABASE_URL` and receive the
 * framework's secret through the sanctioned channel. The list mirrors the
 * names the generated server env schema owns (see the `env.ts` SDK template);
 * the Haskell validator (`Wasp.AppSpec.Valid`) holds the same list, and both
 * sides carry a pointer to the other so they stay in sync.
 *
 * Auth-shaped names (`JWT_SECRET`, OAuth client credentials) are NOT here:
 * they belong to whichever auth package declares them.
 */
export const reservedServerEnvVarNames: readonly string[] = [
  "NODE_ENV",
  "PORT",
  "DATABASE_URL",
  "PG_BOSS_NEW_OPTIONS",
  "WASP_SERVER_URL",
  "WASP_WEB_CLIENT_URL",
  "SMTP_HOST",
  "SMTP_PORT",
  "SMTP_USERNAME",
  "SMTP_PASSWORD",
  "SENDGRID_API_KEY",
  "MAILGUN_API_KEY",
  "MAILGUN_DOMAIN",
  "MAILGUN_API_URL",
  "RESEND_API_KEY",
];

/** Client-side counterpart of {@link reservedServerEnvVarNames}. */
export const reservedClientEnvVarNames: readonly string[] = [
  "NODE_ENV",
  "REACT_APP_API_URL",
];
