import * as z from 'zod';
import type { Result } from '../universal/types';
export declare function ensureEnvSchema<Schema extends z.ZodTypeAny>(data: unknown, schema: Schema): z.infer<Schema>;
export declare function getValidatedEnvOrError<Schema extends z.ZodTypeAny>(env: unknown, schema: Schema): Result<z.infer<Schema>>;
