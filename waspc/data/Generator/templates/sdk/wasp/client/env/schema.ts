{{={= =}=}}
import * as z from "zod"

{=# envValidationSchema.isDefined =}
{=& envValidationSchema.importStatement =}
const userClientEnvSchema = {= envValidationSchema.importIdentifier =};
{=/ envValidationSchema.isDefined =}
{=^ envValidationSchema.isDefined =}
const userClientEnvSchema = z.object({});
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
  "{= serverUrlEnvVarName =}": serverUrlSchema
    .default("{= defaultServerUrl =}"),
});

const waspProdClientEnvSchema = z.object({
  "{= serverUrlEnvVarName =}": serverUrlSchema,
});

const waspClientEnvSchema = import.meta.env.MODE === "production"
  ? waspProdClientEnvSchema
  : waspDevClientEnvSchema;

// PRIVATE API (sdk, Vite config)
export const clientEnvSchema = z.object({
  ...userClientEnvSchema.shape,
  ...waspClientEnvSchema.shape,
});
