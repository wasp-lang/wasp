import * as z from 'zod';
export declare function defineEnvValidationSchema<Schema extends z.ZodObject<any>>(schema: Schema): Schema;
export { ensureEnvSchema } from './validation.js';
