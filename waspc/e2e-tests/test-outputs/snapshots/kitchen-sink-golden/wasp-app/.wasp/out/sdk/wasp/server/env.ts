import * as z from "zod"
import { ensureEnvSchema } from "../env/validation"
import { FromRegister } from "../types/register";
import { serverEnvValidationSchema as serverEnvValidationSchema_ext } from 'virtual:wasp/user/env'

export type RegisteredServerEnvValidationSchema = FromRegister<"serverEnvValidationSchema", z.ZodObject<{}>>;
type UserServerEnvSchema = RegisteredServerEnvValidationSchema;

const userServerEnvSchema: UserServerEnvSchema = serverEnvValidationSchema_ext;

const waspCommonServerEnvSchema = z.object({
  PORT: z.coerce.number().default(3001),
  DATABASE_URL: z.string({
    error: 'DATABASE_URL is required',
  }),
  PG_BOSS_NEW_OPTIONS: z.string().optional(),
  SMTP_HOST: z.string({
    error: getRequiredEnvVarErrorMessage('SMTP email sender', 'SMTP_HOST'),
  }),
  SMTP_PORT: z.coerce.number({
    error: getRequiredEnvVarErrorMessage('SMTP email sender', 'SMTP_PORT'),
  }),
  SMTP_USERNAME: z.string({
    error: getRequiredEnvVarErrorMessage('SMTP email sender', 'SMTP_USERNAME'),
  }),
  SMTP_PASSWORD: z.string({
    error: getRequiredEnvVarErrorMessage('SMTP email sender', 'SMTP_PASSWORD'),
  }),
  "SKIP_EMAIL_VERIFICATION_IN_DEV": z.string({
    error: "SKIP_EMAIL_VERIFICATION_IN_DEV is required by the 'wasp' auth provider: Set to 'true' to skip email verification in development",
  }).optional(),
  "GOOGLE_CLIENT_ID": z.string({
    error: "GOOGLE_CLIENT_ID is required by the 'wasp' auth provider.",
  }),
  "GOOGLE_CLIENT_SECRET": z.string({
    error: "GOOGLE_CLIENT_SECRET is required by the 'wasp' auth provider.",
  }),
  "GITHUB_CLIENT_ID": z.string({
    error: "GITHUB_CLIENT_ID is required by the 'wasp' auth provider.",
  }),
  "GITHUB_CLIENT_SECRET": z.string({
    error: "GITHUB_CLIENT_SECRET is required by the 'wasp' auth provider.",
  }),
  "SLACK_CLIENT_ID": z.string({
    error: "SLACK_CLIENT_ID is required by the 'wasp' auth provider.",
  }),
  "SLACK_CLIENT_SECRET": z.string({
    error: "SLACK_CLIENT_SECRET is required by the 'wasp' auth provider.",
  }),
  "DISCORD_CLIENT_ID": z.string({
    error: "DISCORD_CLIENT_ID is required by the 'wasp' auth provider.",
  }),
  "DISCORD_CLIENT_SECRET": z.string({
    error: "DISCORD_CLIENT_SECRET is required by the 'wasp' auth provider.",
  }),
  "MICROSOFT_CLIENT_ID": z.string({
    error: "MICROSOFT_CLIENT_ID is required by the 'wasp' auth provider.",
  }),
  "MICROSOFT_CLIENT_SECRET": z.string({
    error: "MICROSOFT_CLIENT_SECRET is required by the 'wasp' auth provider.",
  }),
  "MICROSOFT_TENANT_ID": z.string({
    error: "MICROSOFT_TENANT_ID is required by the 'wasp' auth provider.",
  }),
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
  "JWT_SECRET": z.string({
    error: "JWT_SECRET is required by the 'wasp' auth provider: Signs email and OAuth tokens. openssl rand -base64 32",
  }).default("DEVJWTSECRET"),
});

const waspProdServerEnvSchema = z.object({
  NODE_ENV: z.literal("production"),
  "WASP_SERVER_URL": serverUrlSchema,
  "WASP_WEB_CLIENT_URL": clientUrlSchema,
  "JWT_SECRET": z.string({
    error: "JWT_SECRET is required by the 'wasp' auth provider: Signs email and OAuth tokens. openssl rand -base64 32",
  }),
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
