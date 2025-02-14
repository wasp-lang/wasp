import * as z from 'zod';
export declare function ensureEnvSchema<Schema extends z.ZodTypeAny>(data: unknown, schema: Schema): z.infer<Schema>;
export type EnvValidationResult<Env> = {
    type: 'error';
    message: string;
} | {
    type: 'success';
    env: Env;
};
export declare function getValidatedEnvOrError<Schema extends z.ZodTypeAny>(env: unknown, schema: Schema): EnvValidationResult<z.infer<Schema>>;
