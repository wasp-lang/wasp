import * as z from 'zod'

import { clientEnvValidationSchema as clientEnvValidationSchema_ext } from 'wasp/src/env'
const userClientEnvSchema = clientEnvValidationSchema_ext

const serverUrlSchema = z
  .string({
    error: 'REACT_APP_API_URL is required',
  })
  .url({
    error: 'REACT_APP_API_URL must be a valid URL',
  })

const waspClientDevSchema = z.object({
  "REACT_APP_API_URL": serverUrlSchema
    .default('http://localhost:3001'),
})

const waspClientProdSchema = z.object({
  "REACT_APP_API_URL": serverUrlSchema,
})

// PRIVATE API (sdk, Vite config)
export function getClientEnvSchema(mode: string) {
  const waspSchema = mode === 'production' ? waspClientProdSchema : waspClientDevSchema
  return z.object({ ...userClientEnvSchema.shape, ...waspSchema.shape })
}
