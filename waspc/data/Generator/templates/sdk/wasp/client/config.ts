{{={= =}=}}
import { stripTrailingSlash } from '../universal/url.js'
import { env } from './env.js'

{=# isSingleDeployment =}
// TODO: In production the server does not serve the client yet, so for now the
// shared origin below only exists in development.
// The client and the server share one origin, so in the browser the API is
// reached through the page's own origin (in development, the Vite dev server
// proxies the server's routes to the server process). Outside the browser (e.g.
// SSR or prerendering inside the Vite process) there is no `window`, so we fall
// back to the server URL env var if it is set.
const apiUrl = typeof window !== 'undefined'
  ? window.location.origin
  : stripTrailingSlash(env["{= serverUrlEnvVarName =}"] ?? "")
{=/ isSingleDeployment =}
{=^ isSingleDeployment =}
const apiUrl = stripTrailingSlash(env["{= serverUrlEnvVarName =}"])
{=/ isSingleDeployment =}

// PUBLIC API
export type ClientConfig = {
  apiUrl: string,
}

// PUBLIC API
export const config: ClientConfig = {
  apiUrl,
}
