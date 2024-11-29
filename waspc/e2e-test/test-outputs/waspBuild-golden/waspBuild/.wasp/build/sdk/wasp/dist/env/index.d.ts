import * as z from 'zod';
export declare function ensureEnvSchema<Schema extends z.ZodTypeAny>(data: unknown, schema: Schema): z.infer<Schema>;
export type EnvValidationFn = () => z.ZodObject<any>;
