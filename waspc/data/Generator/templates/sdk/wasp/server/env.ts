{{={= =}=}}
import * as z from 'zod'

import { ensureEnvSchema } from '../env/index.js'

const serverCommonSchema = z.object({
  PORT: z.coerce.number().default({= defaultServerPort =}),
  {= databaseUrlEnvVarName =}: z.string({
    required_error: '{= databaseUrlEnvVarName =} is required',
  }),
  PG_BOSS_NEW_OPTIONS: z.string().optional(),
  {=# isEmailSenderEnabled =}
  {=# enabledEmailSenders.isSmtpProviderEnabled =}
  SMTP_HOST: z.string({
    required_error: 'SMTP_HOST is required',
  }),
  SMTP_PORT: z.coerce.number({
    required_error: 'SMTP_PORT is required',
    invalid_type_error: 'SMTP_PORT must be a number',
  }),
  SMTP_USERNAME: z.string({
    required_error: 'SMTP_USERNAME is required',
  }),
  SMTP_PASSWORD: z.string({
    required_error: 'SMTP_PASSWORD is required',
  }),
  {=/ enabledEmailSenders.isSmtpProviderEnabled =}
  {=# enabledEmailSenders.isSendGridProviderEnabled =}
  SENDGRID_API_KEY: z.string({
    required_error: 'SENDGRID_API_KEY is required',
  }),
  {=/ enabledEmailSenders.isSendGridProviderEnabled =}
  {=# enabledEmailSenders.isMailgunProviderEnabled =}
  MAILGUN_API_KEY: z.string({
    required_error: 'MAILGUN_API_KEY is required',
  }),
  MAILGUN_DOMAIN: z.string({
    required_error: 'MAILGUN_DOMAIN is required',
  }),
  MAILGUN_API_URL: z.string().optional(),
  {=/ enabledEmailSenders.isMailgunProviderEnabled =}
  {=/ isEmailSenderEnabled =}
  SKIP_EMAIL_VERIFICATION_IN_DEV: z
    .enum(['true', 'false'], {
      message: 'SKIP_EMAIL_VERIFICATION_IN_DEV must be either "true" or "false"',
    })
    .transform((value) => value === 'true')
    .default('false'),
  {=# isAuthEnabled =}
  {=# enabledAuthProviders.isGoogleAuthEnabled =}
  GOOGLE_CLIENT_ID: z.string({
    required_error: 'GOOGLE_CLIENT_ID is required',
  }),
  GOOGLE_CLIENT_SECRET: z.string({
    required_error: 'GOOGLE_CLIENT_SECRET is required',
  }),
  {=/ enabledAuthProviders.isGoogleAuthEnabled =}
  {=# enabledAuthProviders.isGitHubAuthEnabled =}
  GITHUB_CLIENT_ID: z.string({
    required_error: 'GITHUB_CLIENT_ID is required',
  }),
  GITHUB_CLIENT_SECRET: z.string({
    required_error: 'GITHUB_CLIENT_SECRET is required',
  }),
  {=/ enabledAuthProviders.isGitHubAuthEnabled =}
  {=# enabledAuthProviders.isDiscordAuthEnabled =}
  DISCORD_CLIENT_ID: z.string({
    required_error: 'DISCORD_CLIENT_ID is required',
  }),
  DISCORD_CLIENT_SECRET: z.string({
    required_error: 'DISCORD_CLIENT_SECRET is required',
  }),
  {=/ enabledAuthProviders.isDiscordAuthEnabled =}
  {=# enabledAuthProviders.isKeycloakAuthEnabled =}
  KEYCLOAK_CLIENT_ID: z.string({
    required_error: 'KEYCLOAK_CLIENT_ID is required',
  }),
  KEYCLOAK_CLIENT_SECRET: z.string({
    required_error: 'KEYCLOAK_CLIENT_SECRET is required',
  }),
  KEYCLOAK_REALM_URL: z
    .string({
      required_error: 'KEYCLOAK_REALM_URL is required',
    })
    .url({
      message: 'KEYCLOAK_REALM_URL must be a valid URL',
    }),
  {=/ enabledAuthProviders.isKeycloakAuthEnabled =}
  {=/ isAuthEnabled =}
})

const serverUrlSchema = z
  .string({
    required_error: 'WASP_SERVER_URL is required',
  })
  .url({
    message: 'WASP_SERVER_URL must be a valid URL',
  })

const clientUrlSchema = z
  .string({
    required_error: 'WASP_WEB_CLIENT_URL is required',
  })
  .url({
    message: 'WASP_WEB_CLIENT_URL must be a valid URL',
  })

const jwtTokenSchema = z
  .string({
    required_error: 'JWT_SECRET is required',
  })

// In development, we provide default values for some environment variables
// to make the development process easier.
const serverDevSchema = z.object({
  NODE_ENV: z.literal('development'),
  WASP_SERVER_URL: serverUrlSchema
    .default('{= defaultServerUrl =}'),
  WASP_WEB_CLIENT_URL: clientUrlSchema
    .default('{= defaultClientUrl =}'),
  {=# isAuthEnabled =}
  JWT_SECRET: jwtTokenSchema
    .default('DEVJWTSECRET'),
  {=/ isAuthEnabled =}
})

const serverProdSchema = z.object({
  NODE_ENV: z.literal('production'),
  WASP_SERVER_URL: serverUrlSchema,
  WASP_WEB_CLIENT_URL: clientUrlSchema,
  {=# isAuthEnabled =}
  JWT_SECRET: jwtTokenSchema,
  {=/ isAuthEnabled =}
})

const serverEnvSchema = z.discriminatedUnion('NODE_ENV', [
  serverDevSchema.merge(serverCommonSchema),
  serverProdSchema.merge(serverCommonSchema)
])

// PUBLIC API
export const env = ensureEnvSchema(process.env, serverEnvSchema)
