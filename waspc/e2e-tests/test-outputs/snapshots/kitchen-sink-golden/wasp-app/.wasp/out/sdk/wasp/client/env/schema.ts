import * as z from "zod"
import { FromRegister } from "../../types/register";
import { clientEnvValidationSchema as clientEnvValidationSchema_ext } from 'virtual:wasp/user/env'

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

// In development, the app's server serves the API on the same origin as the
// app itself, so the API's URL is a path (empty when the app is served from the
// root) instead of an absolute URL.
const sameOriginServerUrlSchema =
  z.string()
  .refine((url) => url === "" || url.startsWith("/"), {
    error: 'REACT_APP_API_URL must be a valid URL, or a path when the API is served from the app\'s own origin',
  })

const waspDevClientEnvSchema = z.object({
  "REACT_APP_API_URL": z.union([sameOriginServerUrlSchema, serverUrlSchema])
    .default(""),
});

const waspProdClientEnvSchema = z.object({
  "REACT_APP_API_URL": serverUrlSchema,
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
