{{={= =}=}}
import * as z from "zod"
import type { FromRegister } from "../../types/register";
{=# envValidationSchema.isDefined =}
{=& envValidationSchema.importStatement =}
{=/ envValidationSchema.isDefined =}

export type RegisteredClientEnvValidationSchema = FromRegister<"clientEnvValidationSchema", z.ZodObject<{}>>;
type UserClientEnvSchema = RegisteredClientEnvValidationSchema;

{=# envValidationSchema.isDefined =}
const userClientEnvSchema: UserClientEnvSchema = {= envValidationSchema.importIdentifier =};
{=/ envValidationSchema.isDefined =}
{=^ envValidationSchema.isDefined =}
const userClientEnvSchema: UserClientEnvSchema = z.object({});
{=/ envValidationSchema.isDefined =}

const serverUrlSchema =
  z.string({
    error: '{= serverUrlEnvVarName =} is required',
  })
  .pipe(
    z.url({
      error: '{= serverUrlEnvVarName =} must be a valid URL',
    })
  )

const waspDevClientEnvSchema = z.object({
  "{= serverUrlEnvVarName =}": serverUrlSchema,
});

const waspProdClientEnvSchema = z.object({
  "{= serverUrlEnvVarName =}": serverUrlSchema,
});

// Tests (e.g., with Vitest) run without `wasp start`, which provides the
// server URL in development. They mock the server, so a placeholder is enough.
const waspTestClientEnvSchema = z.object({
  "{= serverUrlEnvVarName =}": serverUrlSchema.default("http://localhost:3001"),
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
