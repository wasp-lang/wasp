{{={= =}=}}
import { stripTrailingSlash, type ClientConfig } from '@wasp.sh/lib-sdk-core'
import { env } from './env.js'

const apiUrl = stripTrailingSlash(env["{= serverUrlEnvVarName =}"])

// PUBLIC API
export const config: ClientConfig = {
  apiUrl,
}
