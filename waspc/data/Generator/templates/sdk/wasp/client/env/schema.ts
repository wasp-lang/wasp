{{={= =}=}}
import * as z from "zod";

{=# userClientEnvSchema.isDefined =}
import { FromRegistry } from "../../types";
// @ts-expect-error
{=& userClientEnvSchema.importStatement =}
export const userClientEnvSchema: FromRegistry<'clientEnvSchema', z.ZodObject> = {= userClientEnvSchema.importIdentifier =};
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

const waspClietEnvSchema = import.meta.env.MODE === "production"
  ? waspProdClientEnvSchema
  : waspDevClientEnvSchema;

// PRIVATE API (sdk, Vite config)
export const clientEnvSchema = z.object({
  ...userClientEnvSchema.shape,
  ...waspClietEnvSchema.shape});
