import type { ZodObject } from 'zod'

export type EnvValidationFn = () => ZodObject<any>
