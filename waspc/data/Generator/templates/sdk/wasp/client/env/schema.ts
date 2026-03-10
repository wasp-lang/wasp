{{={= =}=}}
import * as z from "zod"

{=# envValidationSchema.isDefined =}
{=& envValidationSchema.importStatement =}
export const userClientEnvSchema: typeof {= envValidationSchema.importIdentifier =} = {= envValidationSchema.importIdentifier =};
{=/ envValidationSchema.isDefined =}
{=^ envValidationSchema.isDefined =}
export const userClientEnvSchema = z.object({});
{=/ envValidationSchema.isDefined =}

const serverUrlSchema = z
  .string({
    required_error: "{= serverUrlEnvVarName =} is required",
  })
  .url({
    message: "{= serverUrlEnvVarName =} must be a valid URL",
  });

const waspDevClientEnvSchema = z.object({
  MODE: z.literal("development"),
  "{= serverUrlEnvVarName =}": serverUrlSchema
    .default("{= defaultServerUrl =}"),
});

const waspProdClientEnvSchema = z.object({
  MODE: z.literal("production"),
  "{= serverUrlEnvVarName =}": serverUrlSchema,
});

// PRIVATE API (sdk, Vite config)
export const clientEnvSchema = z.discriminatedUnion("MODE", [
  waspDevClientEnvSchema.merge(userClientEnvSchema),
  waspProdClientEnvSchema.merge(userClientEnvSchema),
]);
