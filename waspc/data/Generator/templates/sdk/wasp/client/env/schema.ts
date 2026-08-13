{{={= =}=}}
import * as z from "zod"
import { FromRegister } from "../../types/register";
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

const serverUrlSchema =
  z.string({
    error: '{= serverUrlEnvVarName =} is required',
  })
  .pipe(
    z.url({
      error: '{= serverUrlEnvVarName =} must be a valid URL',
    })
  )

// The app's server serves the API on the same origin as the app itself, so the
// API's URL is a path (empty when the app is served from the root) instead of
// an absolute URL.
const sameOriginServerUrlSchema =
  z.string()
  .refine((url) => url === "" || url.startsWith("/"), {
    error: '{= serverUrlEnvVarName =} must be a valid URL, or a path when the API is served from the app\'s own origin',
  })

// The default is the app's own origin, which is where its server is. Setting
// this to an absolute URL is for deployments that serve the API from somewhere
// else.
const waspClientEnvSchema = z.object({
  "{= serverUrlEnvVarName =}": z.union([sameOriginServerUrlSchema, serverUrlSchema])
    .default("{= defaultServerUrl =}"),
});

export type CompleteClientEnvSchema = z.ZodObject<typeof waspClientEnvSchema["shape"] & UserClientEnvSchema["shape"]>;

// PRIVATE API (sdk, Vite config)
export const clientEnvSchema: CompleteClientEnvSchema = z.object({
  ...userClientEnvSchema.shape,
  ...waspClientEnvSchema.shape
});
