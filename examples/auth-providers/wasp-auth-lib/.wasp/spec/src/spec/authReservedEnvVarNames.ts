/**
 * Env var names the framework owns and no auth provider manifest may declare.
 *
 * Narrowed adapter runtimes receive exactly the vars their manifest declared,
 * so without this list an adapter could declare `JWT_SECRET` and receive the
 * framework's secret through the sanctioned channel. The list mirrors the
 * names the generated server env schema owns (see the `env.ts` SDK template);
 * the Haskell validator (`Wasp.AppSpec.Valid`) holds the same list, and both
 * sides carry a pointer to the other so they stay in sync.
 *
 * Method-conditional names (OAuth client credentials, email sender
 * credentials) are reserved unconditionally: an adapter squatting
 * `GOOGLE_CLIENT_ID` in an app without Google auth would only start colliding
 * when the app enables the method, which is exactly when the error would be
 * hardest to attribute.
 */
export const reservedServerEnvVarNames: readonly string[] = [
  "NODE_ENV",
  "PORT",
  "DATABASE_URL",
  "PG_BOSS_NEW_OPTIONS",
  "WASP_SERVER_URL",
  "WASP_WEB_CLIENT_URL",
  "JWT_SECRET",
  "SKIP_EMAIL_VERIFICATION_IN_DEV",
  "SMTP_HOST",
  "SMTP_PORT",
  "SMTP_USERNAME",
  "SMTP_PASSWORD",
  "SENDGRID_API_KEY",
  "MAILGUN_API_KEY",
  "MAILGUN_DOMAIN",
  "MAILGUN_API_URL",
  "RESEND_API_KEY",
  "GOOGLE_CLIENT_ID",
  "GOOGLE_CLIENT_SECRET",
  "GITHUB_CLIENT_ID",
  "GITHUB_CLIENT_SECRET",
  "SLACK_CLIENT_ID",
  "SLACK_CLIENT_SECRET",
  "DISCORD_CLIENT_ID",
  "DISCORD_CLIENT_SECRET",
  "KEYCLOAK_CLIENT_ID",
  "KEYCLOAK_CLIENT_SECRET",
  "KEYCLOAK_REALM_URL",
  "MICROSOFT_TENANT_ID",
  "MICROSOFT_CLIENT_ID",
  "MICROSOFT_CLIENT_SECRET",
];

/** Client-side counterpart of {@link reservedServerEnvVarNames}. */
export const reservedClientEnvVarNames: readonly string[] = [
  "NODE_ENV",
  "REACT_APP_API_URL",
];
