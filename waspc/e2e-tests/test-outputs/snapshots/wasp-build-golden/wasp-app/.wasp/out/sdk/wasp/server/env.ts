import * as z from "zod"
import { ensureEnvSchema } from "../env/validation"
import { FromRegister } from "../types/register";

export type RegisteredServerEnvValidationSchema = FromRegister<"serverEnvValidationSchema", z.ZodObject<{}>>;
type UserServerEnvSchema = RegisteredServerEnvValidationSchema;

const userServerEnvSchema: UserServerEnvSchema = z.object({});

const waspCommonServerEnvSchema = z.object({
  PORT: z.coerce.number().default(3001),
  DATABASE_URL: z.string({
    error: 'DATABASE_URL is required',
  }),
  PG_BOSS_NEW_OPTIONS: z.string().optional(),
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

// In development, we provide default values for some environment variables
// to make the development process easier.
const waspDevServerEnvSchema = z.object({
  NODE_ENV: z.literal("development"),
  "WASP_SERVER_URL": serverUrlSchema
    .default("http://localhost:3001"),
  "WASP_WEB_CLIENT_URL": clientUrlSchema
    .default("http://localhost:3000/"),
});

const waspProdServerEnvSchema = z.object({
  NODE_ENV: z.literal("production"),
  "WASP_SERVER_URL": serverUrlSchema,
  "WASP_WEB_CLIENT_URL": clientUrlSchema,
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
