import * as z from 'zod'

import { clientEnvValidationSchema as clientEnvValidationSchema_ext } from 'wasp/src/env'
export const userClientEnvSchema = clientEnvValidationSchema_ext

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
export function getClientWaspEnvSchema(mode: string) {
  return mode === 'production' ? waspClientProdSchema : waspClientDevSchema
}
