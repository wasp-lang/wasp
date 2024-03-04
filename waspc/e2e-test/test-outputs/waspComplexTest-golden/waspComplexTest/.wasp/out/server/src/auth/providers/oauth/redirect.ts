import { config } from 'wasp/server'

export const loginPath = 'login'
export const callbackPath = 'callback'
export const exchangeCodeForTokenPath = 'exchange-code'
const clientOAuthCallbackPath = '/oauth/callback'

export function getRedirectUriForCallback(providerName: string): URL {
  return new URL(`${config.serverUrl}/auth/${providerName}/${callbackPath}`);
}

export function getRedirectUriForOneTimeCode(oneTimeCode: string): URL {
  return new URL(`${config.frontendUrl}${clientOAuthCallbackPath}#${oneTimeCode}`);
}

export function getRedirectUriForError(error: string): URL {
  return new URL(`${config.frontendUrl}${clientOAuthCallbackPath}?error=${error}`);
}
