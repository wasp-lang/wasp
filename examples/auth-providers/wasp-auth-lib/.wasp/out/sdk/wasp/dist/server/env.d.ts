import * as z from "zod";
import { FromRegister } from "../types/register";
export type RegisteredServerEnvValidationSchema = FromRegister<"serverEnvValidationSchema", z.ZodObject<{}>>;
type UserServerEnvSchema = RegisteredServerEnvValidationSchema;
declare const waspServerEnvSchema: z.ZodDiscriminatedUnion<[z.ZodObject<{
    NODE_ENV: z.ZodLiteral<"development">;
    WASP_SERVER_URL: z.ZodDefault<z.ZodPipe<z.ZodString, z.ZodURL>>;
    WASP_WEB_CLIENT_URL: z.ZodDefault<z.ZodPipe<z.ZodString, z.ZodURL>>;
    WASP_AUTH_TOKENS_SECRET: z.ZodDefault<z.ZodString>;
    PORT: z.ZodDefault<z.ZodCoercedNumber<unknown>>;
    DATABASE_URL: z.ZodString;
    PG_BOSS_NEW_OPTIONS: z.ZodOptional<z.ZodString>;
    SKIP_EMAIL_VERIFICATION_IN_DEV: z.ZodPipe<z.ZodDefault<z.ZodEnum<{
        true: "true";
        false: "false";
    }>>, z.ZodTransform<boolean, "true" | "false">>;
    WASP_AUTH_GOOGLE_CLIENT_ID: z.ZodString;
    WASP_AUTH_GOOGLE_CLIENT_SECRET: z.ZodString;
}, z.core.$strip>, z.ZodObject<{
    NODE_ENV: z.ZodLiteral<"production">;
    WASP_SERVER_URL: z.ZodPipe<z.ZodString, z.ZodURL>;
    WASP_WEB_CLIENT_URL: z.ZodPipe<z.ZodString, z.ZodURL>;
    WASP_AUTH_TOKENS_SECRET: z.ZodString;
    PORT: z.ZodDefault<z.ZodCoercedNumber<unknown>>;
    DATABASE_URL: z.ZodString;
    PG_BOSS_NEW_OPTIONS: z.ZodOptional<z.ZodString>;
    SKIP_EMAIL_VERIFICATION_IN_DEV: z.ZodPipe<z.ZodDefault<z.ZodEnum<{
        true: "true";
        false: "false";
    }>>, z.ZodTransform<boolean, "true" | "false">>;
    WASP_AUTH_GOOGLE_CLIENT_ID: z.ZodString;
    WASP_AUTH_GOOGLE_CLIENT_SECRET: z.ZodString;
}, z.core.$strip>], "NODE_ENV">;
type CompleteServerEnvSchema = z.ZodIntersection<UserServerEnvSchema, typeof waspServerEnvSchema>;
export declare const env: z.infer<CompleteServerEnvSchema>;
export {};
//# sourceMappingURL=env.d.ts.map