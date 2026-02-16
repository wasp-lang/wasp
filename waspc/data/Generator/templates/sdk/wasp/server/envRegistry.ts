import * as z from 'zod'

type EnvSchema = z.ZodObject<any>

let _serverEnvSchema: EnvSchema | undefined = undefined

export function registerServerEnvSchema(schema: EnvSchema): void {
  _serverEnvSchema = schema
}

export function getServerEnvSchema(): EnvSchema {
  return _serverEnvSchema ?? z.object({})
}
