{{={= =}=}}
import * as z from "zod"
import { FromRegistry } from "../../types";

type AnyUserClientEnvSchema = z.ZodObject<{}>;
export type UserClientEnvSchema = FromRegistry<"clientEnvSchema", AnyUserClientEnvSchema>

{=# envValidationSchema.isDefined =}
{=& envValidationSchema.importStatement =}
export const userClientEnvSchema: UserClientEnvSchema = {= envValidationSchema.importIdentifier =};
{=/ envValidationSchema.isDefined =}
{=^ envValidationSchema.isDefined =}
export const userClientEnvSchema: UserClientEnvSchema = z.object({});
{=/ envValidationSchema.isDefined =}
