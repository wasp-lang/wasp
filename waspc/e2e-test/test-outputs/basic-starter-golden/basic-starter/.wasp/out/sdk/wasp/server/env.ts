import * as z from 'zod'

import { ensureEnvSchema } from '../env/validation.js'

const userServerEnvSchema = z.object({})

const waspServerCommonSchema = z.object({
  PORT: z.coerce.number().default(3001),
  DATABASE_URL: z.string({
    required_error: 'DATABASE_URL is required',
  }),
  PG_BOSS_NEW_OPTIONS: z.string().optional(),
  SKIP_EMAIL_VERIFICATION_IN_DEV: z
    .enum(['true', 'false'], {
      message: 'SKIP_EMAIL_VERIFICATION_IN_DEV must be either "true" or "false"',
    })
    .transform((value) => value === 'true')
    .default('false'),
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

// PUBLIC API
export const env = ensureEnvSchema(
  { NODE_ENV: serverDevSchema.shape.NODE_ENV.value, ...process.env },
  serverEnvSchema,
)

function getRequiredEnvVarErrorMessage(featureName: string, envVarName: string) {
  return `${envVarName} is required when using ${featureName}`
}
