{{={= =}=}}
import * as z from "zod"

{=# envValidationSchema.isDefined =}
{=& envValidationSchema.importStatement =}
const userClientEnvSchema = {= envValidationSchema.importIdentifier =};
{=/ envValidationSchema.isDefined =}
{=^ envValidationSchema.isDefined =}
const userClientEnvSchema = z.object({});
{=/ envValidationSchema.isDefined =}

const serverUrlSchema = z
  .string({
    required_error: "{= serverUrlEnvVarName =} is required",
  })
  .url({
    message: "{= serverUrlEnvVarName =} must be a valid URL",
  });

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
  const waspClientEnvSchema = mode === 'production' 
    ? waspProdClientEnvSchema
    : waspDevClientEnvSchema;
  return userClientEnvSchema.merge(waspClientEnvSchema);
}
