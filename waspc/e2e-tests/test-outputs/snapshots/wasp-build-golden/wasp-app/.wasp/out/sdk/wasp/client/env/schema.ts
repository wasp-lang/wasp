import * as z from 'zod'

const userClientEnvSchema = z.object({})

const serverUrlSchema = z
  .string({
    required_error: 'REACT_APP_API_URL is required',
  })
  .url({
    message: 'REACT_APP_API_URL must be a valid URL',
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
  return userClientEnvSchema.merge(waspSchema)
}
