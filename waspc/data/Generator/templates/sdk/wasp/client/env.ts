import * as z from 'zod'
import { ensureEnvSchema } from '../env/validation.js'
import { FromRegistry } from '../types/index.js'
import { getClientEnvSchema } from './env/schema.js'

const _env = ensureEnvSchema(import.meta.env, getClientEnvSchema(import.meta.env.MODE))
type ClientEnv = typeof _env & z.infer<FromRegistry<'clientEnvSchema', z.ZodObject<{}>>>
export const env = _env as ClientEnv

// PUBLIC API
// export const env = ensureEnvSchema(import.meta.env, clientEnvSchema)