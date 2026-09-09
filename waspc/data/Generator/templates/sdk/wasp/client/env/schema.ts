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

{=!
  In single deployment mode the client talks to the server through its own origin,
  so the server URL is optional (it is only used outside the browser).
=}
{=# isSingleDeployment =}
const serverUrlSchema =
  z.url({
    error: '{= serverUrlEnvVarName =} must be a valid URL',
  })
  .optional()
{=/ isSingleDeployment =}
{=^ isSingleDeployment =}
const serverUrlSchema =
  z.string({
    error: '{= serverUrlEnvVarName =} is required',
  })
  .pipe(
    z.url({
      error: '{= serverUrlEnvVarName =} must be a valid URL',
    })
  )
{=/ isSingleDeployment =}

const waspDevClientEnvSchema = z.object({
  "{= serverUrlEnvVarName =}": serverUrlSchema,
});

const waspProdClientEnvSchema = z.object({
  "{= serverUrlEnvVarName =}": serverUrlSchema,
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
