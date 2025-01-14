import * as z from 'zod';
export declare function defineEnvValidationSchema<Schema extends z.ZodObject<any>>(schema: Schema): Schema;
export { getValidatedDataOrError, ensureEnvSchema, type SchemaParsingResult, } from './validation.js';
