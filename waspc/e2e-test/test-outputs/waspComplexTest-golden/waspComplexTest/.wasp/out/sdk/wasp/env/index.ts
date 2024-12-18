import * as z from 'zod'

// PUBLIC API
export type EnvValidationFn = () => z.ZodObject<any>

// PRIVATE API (SDK, Vite config)
export { ensureEnvSchema } from './validation.js'
