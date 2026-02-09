{{={= =}=}}
import { stripTrailingSlash } from 'wasp/universal/url'
import { waspEnv } from './waspEnv.js'

const apiUrl = stripTrailingSlash(waspEnv["{= serverUrlEnvVarName =}"])

// PUBLIC API
export type ClientConfig = {
  apiUrl: string,
}

// PUBLIC API
export const config: ClientConfig = {
  apiUrl,
}
