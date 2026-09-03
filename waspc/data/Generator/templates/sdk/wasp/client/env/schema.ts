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

{=# isAuthEnabled =}
{=! Env vars the auth providers' manifests declared for the client. =}
const authProviderEnvSchema = z.object({
  {=# authProviderClientEnvVars =}
  "{= name =}": z.string({
    error: {=& errorJson =},
  }){=# isOptional =}.optional(){=/ isOptional =},
  {=/ authProviderClientEnvVars =}
});

{=/ isAuthEnabled =}
const waspDevClientEnvSchema = z.object({
  "{= serverUrlEnvVarName =}": serverUrlSchema
    .default("{= defaultServerUrl =}"),
{=# isAuthEnabled =}
  ...authProviderEnvSchema.shape,
{=/ isAuthEnabled =}
});

const waspProdClientEnvSchema = z.object({
  "{= serverUrlEnvVarName =}": serverUrlSchema,
{=# isAuthEnabled =}
  ...authProviderEnvSchema.shape,
{=/ isAuthEnabled =}
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
