{{={= =}=}}
import { waspClientEnvSchema } from '../../../core/client/env/waspSchema.js'

{=# envValidationSchema.isDefined =}
{=& envValidationSchema.importStatement =}
const userClientEnvSchema = {= envValidationSchema.importIdentifier =}
{=/ envValidationSchema.isDefined =}
{=^ envValidationSchema.isDefined =}
const userClientEnvSchema = z.object({})
{=/ envValidationSchema.isDefined =}


// PRIVATE API (sdk, Vite config)
export const clientEnvSchema = userClientEnvSchema.merge(waspClientEnvSchema)
