import * as z from "zod";
export declare function ensureEnvSchema<Schema extends z.ZodType>(data: unknown, schema: Schema): z.infer<Schema>;
export declare function getValidatedEnvOrError<Schema extends z.ZodType>(env: unknown, schema: Schema): z.ZodSafeParseResult<z.infer<Schema>>;
export declare function formatZodEnvError(error: z.ZodError): string;
//# sourceMappingURL=validation.d.ts.map