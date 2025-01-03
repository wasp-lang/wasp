import * as z from 'zod';
export type EnvValidationFn = () => z.ZodObject<any>;
export { ensureEnvSchema } from './validation.js';
