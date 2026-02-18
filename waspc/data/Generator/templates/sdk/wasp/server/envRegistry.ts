import * as z from 'zod'

type EnvSchema = z.ZodObject<any>

let _serverEnvSchema: EnvSchema | undefined = undefined

export function registerServerEnvSchema(schema: EnvSchema): void {
  _serverEnvSchema = schema
}

export function getServerEnvSchema(): EnvSchema {
  if (_serverEnvSchema === undefined) {
    throw new Error(
      'Internal Wasp error:\nServer env schema is not registered.'
    )
  }
  return _serverEnvSchema
}
