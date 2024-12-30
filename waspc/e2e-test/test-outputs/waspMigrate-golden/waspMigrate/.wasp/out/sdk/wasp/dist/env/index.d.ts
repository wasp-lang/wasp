import * as z from 'zod';
export declare function ensureEnvSchema<Schema extends z.ZodTypeAny>(data: unknown, schema: Schema): z.infer<Schema>;
export type SchemaParsingResult<Data> = {
    type: 'error';
    message: string;
} | {
    type: 'success';
    data: Data;
};
export declare function getValidatedDataOrError<Schema extends z.ZodTypeAny>(data: unknown, schema: Schema): SchemaParsingResult<z.infer<Schema>>;
