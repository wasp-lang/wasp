import * as z from 'zod'

import { ensureEnvSchema } from '../env/validation.js'

import { serverEnvValidationSchema as serverEnvValidationSchema_ext } from 'wasp/src/env'
const userServerEnvSchema = serverEnvValidationSchema_ext

const waspServerCommonSchema = z.object({
  PORT: z.coerce.number().default(3001),
  DATABASE_URL: z.string({
    required_error: 'DATABASE_URL is required',
  }),
  PG_BOSS_NEW_OPTIONS: z.string().optional(),
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
  SKIP_EMAIL_VERIFICATION_IN_DEV: z
    .enum(['true', 'false'], {
      message: 'SKIP_EMAIL_VERIFICATION_IN_DEV must be either "true" or "false"',
    })
    .transform((value) => value === 'true')
    .default('false'),
  GOOGLE_CLIENT_ID: z.string({
    required_error: getRequiredEnvVarErrorMessage('Google auth provider', 'GOOGLE_CLIENT_ID'),
  }),
  GOOGLE_CLIENT_SECRET: z.string({
    required_error: getRequiredEnvVarErrorMessage('Google auth provider', 'GOOGLE_CLIENT_SECRET'),
  }),
  GITHUB_CLIENT_ID: z.string({
    required_error: getRequiredEnvVarErrorMessage('GitHub auth provider', 'GITHUB_CLIENT_ID'),
  }),
  GITHUB_CLIENT_SECRET: z.string({
    required_error: getRequiredEnvVarErrorMessage('GitHub auth provider', 'GITHUB_CLIENT_SECRET'),
  }),
  SLACK_CLIENT_ID: z.string({
    required_error: getRequiredEnvVarErrorMessage('Slack auth provider', 'SLACK_CLIENT_ID'),
  }),
  SLACK_CLIENT_SECRET: z.string({
    required_error: getRequiredEnvVarErrorMessage('Slack auth provider', 'SLACK_CLIENT_SECRET'),
  }),
  DISCORD_CLIENT_ID: z.string({
    required_error: getRequiredEnvVarErrorMessage('Discord auth provider', 'DISCORD_CLIENT_ID'),
  }),
  DISCORD_CLIENT_SECRET: z.string({
    required_error: getRequiredEnvVarErrorMessage('Discord auth provider', 'DISCORD_CLIENT_SECRET'),
  }),
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
  "WASP_SERVER_URL": serverUrlSchema
    .default('http://localhost:3001'),
  "WASP_WEB_CLIENT_URL": clientUrlSchema
    .default('http://localhost:3000/'),
  "JWT_SECRET": jwtTokenSchema
    .default('DEVJWTSECRET'),
})

const serverProdSchema = z.object({
  NODE_ENV: z.literal('production'),
  "WASP_SERVER_URL": serverUrlSchema,
  "WASP_WEB_CLIENT_URL": clientUrlSchema,
  "JWT_SECRET": jwtTokenSchema,
})

const serverCommonSchema = userServerEnvSchema.merge(waspServerCommonSchema)
const serverEnvSchema = z.discriminatedUnion('NODE_ENV', [
  serverDevSchema.merge(serverCommonSchema),
  serverProdSchema.merge(serverCommonSchema)
])

const defaultNodeEnvValue = serverDevSchema.shape.NODE_ENV.value;
const { NODE_ENV: inputNodeEnvValue, ...restEnv } = process.env;
// PUBLIC API
export const env = ensureEnvSchema(
  {
    NODE_ENV: inputNodeEnvValue ?? defaultNodeEnvValue,
    ...restEnv,
  },
  serverEnvSchema,
)

function getRequiredEnvVarErrorMessage(featureName: string, envVarName: string) {
  return `${envVarName} is required when using ${featureName}`
}
