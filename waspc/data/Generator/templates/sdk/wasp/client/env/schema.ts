{{={= =}=}}
import * as z from "zod"

{=# userClientEnvSchema.isDefined =}
{=& userClientEnvSchema.importStatement =}
export const userClientEnvSchema: typeof {= userClientEnvSchema.importIdentifier =} = {= userClientEnvSchema.importIdentifier =};
{=/ userClientEnvSchema.isDefined =}
{=^ userClientEnvSchema.isDefined =}
export const userClientEnvSchema = z.object({});
{=/ userClientEnvSchema.isDefined =}

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
  waspProdClientEnvSchema.merge(userClientEnvSchema)
]);

