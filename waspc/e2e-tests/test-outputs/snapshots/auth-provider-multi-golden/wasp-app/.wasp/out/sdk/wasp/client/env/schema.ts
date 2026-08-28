import * as z from "zod"
import { FromRegister } from "../../types/register";

export type RegisteredClientEnvValidationSchema = FromRegister<"clientEnvValidationSchema", z.ZodObject<{}>>;
type UserClientEnvSchema = RegisteredClientEnvValidationSchema;

const userClientEnvSchema: UserClientEnvSchema = z.object({});

const serverUrlSchema =
  z.string({
    error: 'REACT_APP_API_URL is required',
  })
  .pipe(
    z.url({
      error: 'REACT_APP_API_URL must be a valid URL',
    })
  )

const externalAuthProviderEnvSchema = z.object({
  "REACT_APP_CLERK_PUBLISHABLE_KEY": z.string({
    error: "REACT_APP_CLERK_PUBLISHABLE_KEY is required by the 'external:clerk' auth provider: Clerk dashboard → API keys (publishable key)",
  }),
});

const waspDevClientEnvSchema = z.object({
  "REACT_APP_API_URL": serverUrlSchema
    .default("http://localhost:3001"),
  ...externalAuthProviderEnvSchema.shape,
});

const waspProdClientEnvSchema = z.object({
  "REACT_APP_API_URL": serverUrlSchema,
  ...externalAuthProviderEnvSchema.shape,
});

const waspClientEnvSchema = import.meta.env.MODE === "production"
  ? waspProdClientEnvSchema
  : waspDevClientEnvSchema;

export type CompleteClientEnvSchema = z.ZodObject<typeof waspClientEnvSchema["shape"] & UserClientEnvSchema["shape"]>;

// PRIVATE API (sdk, Vite config)
export const clientEnvSchema: CompleteClientEnvSchema = z.object({
  ...userClientEnvSchema.shape,
  ...waspClientEnvSchema.shape
});
