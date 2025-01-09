{{={= =}=}}
import * as z from 'zod'

import { ensureEnvSchema } from '../env/validation.js'

{=# envValidationSchema.isDefined =}
{=& envValidationSchema.importStatement =}
const userServerEnvSchema = {= envValidationSchema.importIdentifier =}
{=/ envValidationSchema.isDefined =}
{=^ envValidationSchema.isDefined =}
const userServerEnvSchema = z.object({})
{=/ envValidationSchema.isDefined =}

const waspServerCommonSchema = z.object({
  PORT: z.coerce.number().default({= defaultServerPort =}),
  {= databaseUrlEnvVarName =}: z.string({
    required_error: '{= databaseUrlEnvVarName =} is required',
  }),
  PG_BOSS_NEW_OPTIONS: z.string().optional(),
  {=# isEmailSenderEnabled =}
  {=# enabledEmailSenders.isSmtpProviderEnabled =}
  SMTP_HOST: z.string({
    required_error: getRequiredEnvVarErrorMessage('SMTP email sender', 'SMTP_HOST'),
  }),
  SMTP_PORT: z.coerce.number({
    required_error: getRequiredEnvVarErrorMessage('SMTP email sender', 'SMTP_PORT'),
    invalid_type_error: 'SMTP_PORT must be a number',
  }),
  SMTP_USERNAME: z.string({
    required_error: getRequiredEnvVarErrorMessage('SMTP email sender', 'SMTP_USERNAME'),
  }),
  SMTP_PASSWORD: z.string({
    required_error: getRequiredEnvVarErrorMessage('SMTP email sender', 'SMTP_PASSWORD'),
  }),
  {=/ enabledEmailSenders.isSmtpProviderEnabled =}
  {=# enabledEmailSenders.isSendGridProviderEnabled =}
  SENDGRID_API_KEY: z.string({
    required_error: getRequiredEnvVarErrorMessage('SendGrid email sender', 'SENDGRID_API_KEY'),
  }),
  {=/ enabledEmailSenders.isSendGridProviderEnabled =}
  {=# enabledEmailSenders.isMailgunProviderEnabled =}
  MAILGUN_API_KEY: z.string({
    required_error: getRequiredEnvVarErrorMessage('Mailgun email sender', 'MAILGUN_API_KEY'),
  }),
  MAILGUN_DOMAIN: z.string({
    required_error: getRequiredEnvVarErrorMessage('Mailgun email sender', 'MAILGUN_DOMAIN'),
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
    required_error: getRequiredEnvVarErrorMessage('Google auth provider', 'GOOGLE_CLIENT_ID'),
  }),
  GOOGLE_CLIENT_SECRET: z.string({
    required_error: getRequiredEnvVarErrorMessage('Google auth provider', 'GOOGLE_CLIENT_SECRET'),
  }),
  {=/ enabledAuthProviders.isGoogleAuthEnabled =}
  {=# enabledAuthProviders.isGitHubAuthEnabled =}
  GITHUB_CLIENT_ID: z.string({
    required_error: getRequiredEnvVarErrorMessage('GitHub auth provider', 'GITHUB_CLIENT_ID'),
  }),
  GITHUB_CLIENT_SECRET: z.string({
    required_error: getRequiredEnvVarErrorMessage('GitHub auth provider', 'GITHUB_CLIENT_SECRET'),
  }),
  {=/ enabledAuthProviders.isGitHubAuthEnabled =}
  {=# enabledAuthProviders.isDiscordAuthEnabled =}
  DISCORD_CLIENT_ID: z.string({
    required_error: getRequiredEnvVarErrorMessage('Discord auth provider', 'DISCORD_CLIENT_ID'),
  }),
  DISCORD_CLIENT_SECRET: z.string({
    required_error: getRequiredEnvVarErrorMessage('Discord auth provider', 'DISCORD_CLIENT_SECRET'),
  }),
  {=/ enabledAuthProviders.isDiscordAuthEnabled =}
  {=# enabledAuthProviders.isKeycloakAuthEnabled =}
  KEYCLOAK_CLIENT_ID: z.string({
    required_error: getRequiredEnvVarErrorMessage('Keycloak auth provider', 'KEYCLOAK_CLIENT_ID'),
  }),
  KEYCLOAK_CLIENT_SECRET: z.string({
    required_error: getRequiredEnvVarErrorMessage('Keycloak auth provider', 'KEYCLOAK_CLIENT_SECRET'),
  }),
  KEYCLOAK_REALM_URL: z
    .string({
      required_error: getRequiredEnvVarErrorMessage('Keycloak auth provider', 'KEYCLOAK_REALM_URL'),
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

{=# isAuthEnabled =}
const jwtTokenSchema = z
  .string({
    required_error: 'JWT_SECRET is required',
  })
{=/ isAuthEnabled =}

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

const serverCommonSchema = userServerEnvSchema.merge(waspServerCommonSchema)
const serverEnvSchema = z.discriminatedUnion('NODE_ENV', [
  serverDevSchema.merge(serverCommonSchema),
  serverProdSchema.merge(serverCommonSchema)
])

// PUBLIC API
export const env = ensureEnvSchema(process.env, serverEnvSchema)

function getRequiredEnvVarErrorMessage(featureName: string, envVarName: string) {
  return `${envVarName} is required when using ${featureName}`
}
