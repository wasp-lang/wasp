{{={= =}=}}
import * as z from 'zod'
import { getClientEnvSchema } from './envRegistry.js'
import type { GetConfigFromRegistry } from 'wasp/types'

type UserClientEnvSchema = GetConfigFromRegistry<'clientEnvSchema', z.ZodObject<{}>>

const waspClientEnvSchema = z.object({
  "{= serverUrlEnvVarName =}": z
  .string()
  .url({
    message: '{= serverUrlEnvVarName =} must be a valid URL',
  })
  .default('{= defaultServerUrl =}'),
})

// PRIVATE API (sdk, Vite config)

type ClientEnvSchema = typeof waspClientEnvSchema
    & UserClientEnvSchema

let _cached: ClientEnvSchema | undefined

// Lazy Proxy: defers env validation to first property access.
export const clientEnvSchema: UserClientEnvSchema = new Proxy({} as ClientEnvSchema, {
  get(_, prop, receiver) {
    if (!_cached) {
      const userClientEnvSchema = getClientEnvSchema() as UserClientEnvSchema
      _cached = userClientEnvSchema.merge(waspClientEnvSchema) as ClientEnvSchema
    }
    return Reflect.get(_cached!, prop, receiver)
  },
})

