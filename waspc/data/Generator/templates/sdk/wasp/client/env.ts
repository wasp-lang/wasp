import * as z from 'zod'
import { ensureEnvSchema } from '../env/validation.js'
import { GetConfigFromRegistry } from '../types/index.js'
import { clientEnvSchema } from './env/schema.js'

const _env = ensureEnvSchema(import.meta.env, clientEnvSchema)
type ClientEnv = typeof _env & z.infer<GetConfigFromRegistry<'clientEnvSchema', z.ZodObject<{}>>>
export const env = _env as ClientEnv

// PUBLIC API
// export const env = ensureEnvSchema(import.meta.env, clientEnvSchema)