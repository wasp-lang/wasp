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

const waspClientEnvSchema = import.meta.env.MODE === "production"
  ? waspProdClientEnvSchema
  : waspDevClientEnvSchema;

// PRIVATE API (sdk, Vite config)
export const clientEnvSchema = z.object({
  ...userClientEnvSchema.shape,
  ...waspClientEnvSchema.shape,
});
