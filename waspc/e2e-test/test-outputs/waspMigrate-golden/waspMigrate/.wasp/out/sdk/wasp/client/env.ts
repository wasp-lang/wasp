import * as z from 'zod'

import { ensureEnvSchema } from '../env/validation.js'

const userClientEnvSchema = z.object({})

const waspClientEnvSchema = z.object({
  REACT_APP_API_URL: z
    .string({
      required_error: 'REACT_APP_API_URL is required',
    })
    .default('http://localhost:3001')
})

const clientEnvSchema = userClientEnvSchema.merge(waspClientEnvSchema)

// PUBLIC API
export const env = ensureEnvSchema(import.meta.env, clientEnvSchema)
