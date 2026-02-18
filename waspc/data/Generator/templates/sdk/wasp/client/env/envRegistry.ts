import * as z from 'zod'

type EnvSchema = z.ZodObject<any>

let _clientEnvSchema: EnvSchema | undefined = undefined

export function registerClientEnvSchema(schema: EnvSchema): void {
  _clientEnvSchema = schema
}

export function getClientEnvSchema(): EnvSchema {
  if (_clientEnvSchema === undefined) {
    throw new Error(
      'Internal Wasp error:\nClient env schema is not registered.' 
    )
  }
  return _clientEnvSchema
}
