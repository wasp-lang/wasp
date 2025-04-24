import * as z from 'zod';
export declare function ensureEnvSchema<Schema extends z.ZodTypeAny>(data: unknown, schema: Schema): z.infer<Schema>;
export declare function getValidatedEnvOrError<Schema extends z.ZodTypeAny>(env: unknown, schema: Schema): z.SafeParseReturnType<unknown, z.infer<Schema>>;
export declare function formatZodEnvErrors(issues: z.ZodIssue[]): string;
//# sourceMappingURL=validation.d.ts.map