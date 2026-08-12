import * as z from "zod"
import { FromRegister } from "../../types/register";

export type RegisteredClientEnvValidationSchema = FromRegister<"clientEnvValidationSchema", z.ZodObject<{}>>;
type UserClientEnvSchema = RegisteredClientEnvValidationSchema;

const userClientEnvSchema: UserClientEnvSchema = z.object({});

const serverUrlSchema =
  z.string({
    error: 'REACT_APP_API_URL is required',
  })
  .pipe(
    z.url({
      error: 'REACT_APP_API_URL must be a valid URL',
    })
  )

// The app's server serves the API on the same origin as the app itself, so the
// API's URL is a path (empty when the app is served from the root) instead of
// an absolute URL.
const sameOriginServerUrlSchema =
  z.string()
  .refine((url) => url === "" || url.startsWith("/"), {
    error: 'REACT_APP_API_URL must be a valid URL, or a path when the API is served from the app\'s own origin',
  })

// The default is the app's own origin, which is where its server is. Setting
// this to an absolute URL is for deployments that serve the API from somewhere
// else.
const waspClientEnvSchema = z.object({
  "REACT_APP_API_URL": z.union([sameOriginServerUrlSchema, serverUrlSchema])
    .default(""),
});

export type CompleteClientEnvSchema = z.ZodObject<typeof waspClientEnvSchema["shape"] & UserClientEnvSchema["shape"]>;

// PRIVATE API (sdk, Vite config)
export const clientEnvSchema: CompleteClientEnvSchema = z.object({
  ...userClientEnvSchema.shape,
  ...waspClientEnvSchema.shape
});
