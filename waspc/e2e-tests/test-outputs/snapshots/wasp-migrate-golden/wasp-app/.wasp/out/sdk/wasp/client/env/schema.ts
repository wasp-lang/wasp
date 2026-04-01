import * as z from "zod"

const userClientEnvSchema = z.object({});

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
  "REACT_APP_API_URL": serverUrlSchema
    .default("http://localhost:3001"),
});

const waspProdClientEnvSchema = z.object({
  "REACT_APP_API_URL": serverUrlSchema,
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
