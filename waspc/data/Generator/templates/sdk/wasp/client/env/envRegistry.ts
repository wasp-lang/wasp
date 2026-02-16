import * as z from 'zod'

type EnvSchema = z.ZodObject<any>

let _clientEnvSchema: EnvSchema | undefined = undefined

export function registerClientEnvSchema(schema: EnvSchema): void {
  _clientEnvSchema = schema
}

export function getClientEnvSchema(): EnvSchema {
  return _clientEnvSchema ?? z.object({})
}
