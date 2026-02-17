import type { GetFromRegister } from 'wasp/types'
import type * as z from 'zod'
import { ensureEnvSchema } from '../env/validation.js'
import { clientEnvSchema } from './env/schema.js'

const _env = ensureEnvSchema(import.meta.env, clientEnvSchema)

type ClientEnv = typeof _env & z.infer<GetFromRegister<'clientEnvSchema', z.ZodObject<{}>>>

// PUBLIC API
export const env = _env as ClientEnv
