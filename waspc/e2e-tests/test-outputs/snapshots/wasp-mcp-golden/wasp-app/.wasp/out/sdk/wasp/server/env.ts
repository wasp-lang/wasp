import * as z from "zod"
import { ensureEnvSchema } from "@wasp/lib-sdk-core"
import type { FromRegister } from "../types/register";

export type RegisteredServerEnvValidationSchema = FromRegister<"serverEnvValidationSchema", z.ZodObject<{}>>;
type UserServerEnvSchema = RegisteredServerEnvValidationSchema;

const userServerEnvSchema: UserServerEnvSchema = z.object({});

const waspCommonServerEnvSchema = z.object({
  PORT: z.coerce.number({
    error: 'PORT is required and must be a number',
  }),
  DATABASE_URL: z.string({
    error: 'DATABASE_URL is required',
  }),
  PG_BOSS_NEW_OPTIONS: z.string().optional(),
  SKIP_EMAIL_VERIFICATION_IN_DEV: z
    .enum(['true', 'false'], {
      error: 'SKIP_EMAIL_VERIFICATION_IN_DEV must be either "true" or "false"',
    })
    .default('false')
    .transform((value) => value === 'true'),
});

const serverUrlSchema =
  z.string({
    error: 'WASP_SERVER_URL is required',
  })
  .pipe(
    z.url({
      error: 'WASP_SERVER_URL must be a valid URL',
    })
  )

const clientUrlSchema =
  z.string({
    error: 'WASP_WEB_CLIENT_URL is required',
  })
  .pipe(
    z.url({
      error: 'WASP_WEB_CLIENT_URL must be a valid URL',
    })
  )

const jwtTokenSchema = z
  .string({
    error: 'JWT_SECRET is required',
  })

// Seals MCP OAuth codes and tokens. Changing it signs every MCP client out.
const mcpSecretSchema = z
  .string({
    error: 'MCP_SECRET is required (generate one with `openssl rand -base64 32`)',
  })
  .refine((value) => Buffer.byteLength(value, 'utf8') >= 32, {
    error: 'MCP_SECRET must be at least 32 bytes (generate one with `openssl rand -base64 32`)',
  })

// In development, we provide default values for some environment variables
// to make the development process easier.
const waspDevServerEnvSchema = z.object({
  NODE_ENV: z.literal("development"),
  "WASP_SERVER_URL": serverUrlSchema,
  "WASP_WEB_CLIENT_URL": clientUrlSchema,
  "JWT_SECRET": jwtTokenSchema
    .default("DEVJWTSECRET"),
  "MCP_SECRET": mcpSecretSchema
    .default("DEV-ONLY-MCP-SECRET-0123456789abcdef"),
});

const waspProdServerEnvSchema = z.object({
  NODE_ENV: z.literal("production"),
  "WASP_SERVER_URL": serverUrlSchema,
  "WASP_WEB_CLIENT_URL": clientUrlSchema,
  "JWT_SECRET": jwtTokenSchema,
  "MCP_SECRET": mcpSecretSchema,
});

const waspServerEnvSchema = z.discriminatedUnion("NODE_ENV", [
  z.object({...waspCommonServerEnvSchema.shape, ...waspDevServerEnvSchema.shape}),
  z.object({...waspCommonServerEnvSchema.shape, ...waspProdServerEnvSchema.shape}),
]);

type CompleteServerEnvSchema = z.ZodIntersection<UserServerEnvSchema, typeof waspServerEnvSchema>;

const serverEnvSchema: CompleteServerEnvSchema = userServerEnvSchema.and(waspServerEnvSchema);

const defaultNodeEnvValue = waspDevServerEnvSchema.shape.NODE_ENV.value;
const { NODE_ENV: inputNodeEnvValue, ...restEnv } = process.env;

// PUBLIC API
export const env: z.infer<CompleteServerEnvSchema> = ensureEnvSchema(
  {
    NODE_ENV: inputNodeEnvValue ?? defaultNodeEnvValue,
    ...restEnv,
  },
  serverEnvSchema,
);

function getRequiredEnvVarErrorMessage(featureName: string, envVarName: string) {
  return `${envVarName} is required when using ${featureName}`
}
