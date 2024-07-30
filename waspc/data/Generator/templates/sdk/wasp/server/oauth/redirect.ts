{{={= =}=}}
import { config } from '../../server/index.js'

// PRIVATE API
export const callbackPath = '{= serverOAuthCallbackHandlerPath =}'

// PRIVATE API
export function getRedirectUriForCallback(providerName: string): URL {
  return new URL(`${config.serverUrl}/auth/${providerName}/${callbackPath}`);
}
