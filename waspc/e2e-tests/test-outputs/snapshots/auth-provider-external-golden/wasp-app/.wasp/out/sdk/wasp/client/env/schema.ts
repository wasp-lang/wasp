import * as z from "zod"
import { FromRegister } from "../../types/register";
import { clientEnvSchema as clientEnvSchema_ext } from 'virtual:wasp/user/env'

export type RegisteredClientEnvValidationSchema = FromRegister<"clientEnvValidationSchema", z.ZodObject<{}>>;
type UserClientEnvSchema = RegisteredClientEnvValidationSchema;

const userClientEnvSchema: UserClientEnvSchema = clientEnvSchema_ext;

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
