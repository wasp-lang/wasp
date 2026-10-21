import * as z from "zod"
import type { FromRegister } from "../../types/register";
import { clientEnvValidationSchema as clientEnvValidationSchema_ext } from "virtual:wasp/user/env"

export type RegisteredClientEnvValidationSchema = FromRegister<"clientEnvValidationSchema", z.ZodObject<{}>>;
type UserClientEnvSchema = RegisteredClientEnvValidationSchema;

const userClientEnvSchema: UserClientEnvSchema = clientEnvValidationSchema_ext;

const serverUrlSchema =
  z.string({
    error: 'REACT_APP_API_URL is required',
  })
  .pipe(
    z.url({
      error: 'REACT_APP_API_URL must be a valid URL',
    })
  )

const waspDevClientEnvSchema = z.object({
  "REACT_APP_API_URL": serverUrlSchema,
});

const waspProdClientEnvSchema = z.object({
  "REACT_APP_API_URL": serverUrlSchema,
});

// Tests (e.g., with Vitest) run without `wasp start`, which provides the
// server URL in development. They mock the server, so a placeholder is enough.
const waspTestClientEnvSchema = z.object({
  "REACT_APP_API_URL": serverUrlSchema.default("http://localhost:3001"),
});

const waspClientEnvSchema = import.meta.env.MODE === "production"
  ? waspProdClientEnvSchema
  : import.meta.env.MODE === "test"
    ? waspTestClientEnvSchema
    : waspDevClientEnvSchema;

export type CompleteClientEnvSchema = z.ZodObject<typeof waspClientEnvSchema["shape"] & UserClientEnvSchema["shape"]>;

// PRIVATE API (sdk, Vite config)
export const clientEnvSchema: CompleteClientEnvSchema = z.object({
  ...userClientEnvSchema.shape,
  ...waspClientEnvSchema.shape
});
