{{={= =}=}}
import * as z from "zod";

{=# userClientEnvSchema.isDefined =}
{=& userClientEnvSchema.importStatement =}
export const userClientEnvSchema: typeof {= userClientEnvSchema.importIdentifier =} = {= userClientEnvSchema.importIdentifier =};
{=/ userClientEnvSchema.isDefined =}
{=^ userClientEnvSchema.isDefined =}
export const userClientEnvSchema = z.object({});
{=/ userClientEnvSchema.isDefined =}

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

// PRIVATE API (sdk, Vite config)
// TODO(franjo): Remove passing mode as param when this is no longer a plugin.
//               See: https://github.com/wasp-lang/wasp/issues/3875.
export function getClientEnvSchema(mode: string) {
  const waspClientEnvSchema = mode === "production"
    ? waspProdClientEnvSchema
    : waspDevClientEnvSchema;
  return z.object({ ...userClientEnvSchema.shape, ...waspClientEnvSchema.shape })
}
