import * as z from "zod";
import { FromRegister } from "../../types/register";
export type RegisteredClientEnvValidationSchema = FromRegister<"clientEnvValidationSchema", z.ZodObject<{}>>;
type UserClientEnvSchema = RegisteredClientEnvValidationSchema;
declare const waspClientEnvSchema: z.ZodObject<{
    REACT_APP_API_URL: z.ZodDefault<z.ZodPipe<z.ZodString, z.ZodURL>>;
}, z.core.$strip> | z.ZodObject<{
    REACT_APP_API_URL: z.ZodPipe<z.ZodString, z.ZodURL>;
}, z.core.$strip>;
export type CompleteClientEnvSchema = z.ZodObject<typeof waspClientEnvSchema["shape"] & UserClientEnvSchema["shape"]>;
export declare const clientEnvSchema: CompleteClientEnvSchema;
export {};
//# sourceMappingURL=schema.d.ts.map