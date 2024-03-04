{{={= =}=}}
import { config } from 'wasp/server'

export const loginPath = '{= serverOAuthLoginHandlerPath =}'
export const callbackPath = '{= serverOAuthCallbackHandlerPath =}'
export const exchangeCodeForTokenPath = '{= serverExchangeCodeForTokenHandlerPath =}'
const clientOAuthCallbackPath = '{= clientOAuthCallbackPath =}'

export function getRedirectUriForCallback(providerName: string): URL {
  return new URL(`${config.serverUrl}/auth/${providerName}/${callbackPath}`);
}

export function getRedirectUriForOneTimeCode(oneTimeCode: string): URL {
  return new URL(`${config.frontendUrl}${clientOAuthCallbackPath}#${oneTimeCode}`);
}

export function getRedirectUriForError(error: string): URL {
  return new URL(`${config.frontendUrl}${clientOAuthCallbackPath}?error=${error}`);
}
