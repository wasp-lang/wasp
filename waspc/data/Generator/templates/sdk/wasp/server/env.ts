{{={= =}=}}
import * as z from 'zod'

import { ensureEnvSchema } from '../env/validation.js'
import { getServerEnvSchema } from './envRegistry.js'
import type { Register } from 'wasp/types'

type UserServerEnvSchema =
  Register extends { serverEnvSchema: infer T extends z.ZodTypeAny }
    ? T
    : z.ZodObject<{}>

const userServerEnvSchema = getServerEnvSchema() as UserServerEnvSchema

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
  {=# enabledAuthProviders.isSlackAuthEnabled =}
  SLACK_CLIENT_ID: z.string({
    required_error: getRequiredEnvVarErrorMessage('Slack auth provider', 'SLACK_CLIENT_ID'),
  }),
  SLACK_CLIENT_SECRET: z.string({
    required_error: getRequiredEnvVarErrorMessage('Slack auth provider', 'SLACK_CLIENT_SECRET'),
  }),
  {=/ enabledAuthProviders.isSlackAuthEnabled =}
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
  {=# enabledAuthProviders.isMicrosoftAuthEnabled =}
  MICROSOFT_TENANT_ID: z.string({
    required_error: getRequiredEnvVarErrorMessage('Microsoft auth provider', 'MICROSOFT_TENANT_ID'),
  }),
  MICROSOFT_CLIENT_ID: z.string({
    required_error: getRequiredEnvVarErrorMessage('Microsoft auth provider', 'MICROSOFT_CLIENT_ID'),
  }),
  MICROSOFT_CLIENT_SECRET: z.string({
    required_error: getRequiredEnvVarErrorMessage('Microsoft auth provider', 'MICROSOFT_CLIENT_SECRET'),
  }),
  {=/ enabledAuthProviders.isMicrosoftAuthEnabled =}
  {=/ isAuthEnabled =}
})

const serverUrlSchema = z
  .string({
    required_error: '{= serverUrlEnvVarName =} is required',
  })
  .url({
    message: '{= serverUrlEnvVarName =} must be a valid URL',
  })

const clientUrlSchema = z
  .string({
    required_error: '{= clientUrlEnvVarName =} is required',
  })
  .url({
    message: '{= clientUrlEnvVarName =} must be a valid URL',
  })

{=# isAuthEnabled =}
const jwtTokenSchema = z
  .string({
    required_error: '{= jwtSecretEnvVarName =} is required',
  })
{=/ isAuthEnabled =}

// In development, we provide default values for some environment variables
// to make the development process easier.
const serverDevSchema = z.object({
  NODE_ENV: z.literal('development'),
  "{= serverUrlEnvVarName =}": serverUrlSchema
    .default('{= defaultServerUrl =}'),
  "{= clientUrlEnvVarName =}": clientUrlSchema
    .default('{= defaultClientUrl =}'),
  {=# isAuthEnabled =}
  "{= jwtSecretEnvVarName =}": jwtTokenSchema
    .default('DEVJWTSECRET'),
  {=/ isAuthEnabled =}
})

const serverProdSchema = z.object({
  NODE_ENV: z.literal('production'),
  "{= serverUrlEnvVarName =}": serverUrlSchema,
  "{= clientUrlEnvVarName =}": clientUrlSchema,
  {=# isAuthEnabled =}
  "{= jwtSecretEnvVarName =}": jwtTokenSchema,
  {=/ isAuthEnabled =}
})

const serverCommonSchema = userServerEnvSchema.merge(waspServerCommonSchema)
const serverEnvSchema = z.discriminatedUnion('NODE_ENV', [
  serverDevSchema.merge(serverCommonSchema),
  serverProdSchema.merge(serverCommonSchema)
])

const defaultNodeEnvValue = serverDevSchema.shape.NODE_ENV.value;
const { NODE_ENV: inputNodeEnvValue, ...restEnv } = process.env;

const _env = ensureEnvSchema(
  {
    NODE_ENV: inputNodeEnvValue ?? defaultNodeEnvValue,
    ...restEnv,
  },
  serverEnvSchema,
)

// Intersect the concrete wasp env type with user-defined env vars from Register.
// The conditional is preserved in .d.ts so consumers get the full type.
type ServerEnv = typeof _env &
  (Register extends { serverEnvSchema: infer T extends z.ZodTypeAny }
    ? z.infer<T>
    : {})

// PUBLIC API
export const env = _env as ServerEnv

function getRequiredEnvVarErrorMessage(featureName: string, envVarName: string) {
  return `${envVarName} is required when using ${featureName}`
}
