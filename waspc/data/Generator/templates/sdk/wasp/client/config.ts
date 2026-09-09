{{={= =}=}}
import { stripTrailingSlash } from '../universal/url.js'
import { env } from './env.js'

const serverOrigin = stripTrailingSlash(env["{= serverUrlEnvVarName =}"])

// The URL of the API, including `server.basePath`. Every server route
// (Wasp's own and the user's apis) is reached by appending its path to it.
const apiUrl = `${serverOrigin}{=& serverBasePathPrefix =}`

// PUBLIC API
export type ClientConfig = {
  apiUrl: string,
}

// PUBLIC API
export const config: ClientConfig = {
  apiUrl,
}
