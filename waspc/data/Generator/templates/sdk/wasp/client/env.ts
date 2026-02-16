import { clientEnvSchema } from './env/schema.js'
import { ensureEnvSchema } from '../env/validation.js'
import type { Register } from 'wasp/types'
import type * as z from 'zod'

const _env = ensureEnvSchema(import.meta.env, clientEnvSchema)

// Intersect the concrete wasp env type with user-defined env vars from Register.
// The conditional is preserved in .d.ts so consumers get the full type.
type ClientEnv = typeof _env &
  (Register extends { clientEnvSchema: infer T extends z.ZodTypeAny }
    ? z.infer<T>
    : {})

// PUBLIC API
export const env = _env as ClientEnv
