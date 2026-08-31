{{={= =}=}}
import { config, HttpError } from '../../index.js'
import { appDelivery } from '../../core/delivery.js'

// PRIVATE API (server)
export const providerLoginPath = '{= providerLoginPath =}'

// PRIVATE API (server)
{=# usesSessionHandoff =}
export const sessionHandoffExchangePath = '{= sessionHandoffExchangePath =}'
{=/ usesSessionHandoff =}

// PRIVATE API (server)
export const providerCallbackPath = '{= providerCallbackPath =}'

const oauthLoginResultPath = '{= oauthLoginResultPath =}'

// PRIVATE API (server)
export function getOAuthLoginResultUrl(): URL {
  return new URL(`${config.frontendUrl}${oauthLoginResultPath}`)
}

{=# usesSessionHandoff =}
// PRIVATE API (server)
export function getOAuthLoginResultUrlWithSessionHandoffCode(sessionHandoffCode: string): URL {
  const resultUrl = getOAuthLoginResultUrl()
  resultUrl.hash = sessionHandoffCode
  return resultUrl
}
{=/ usesSessionHandoff =}

// PRIVATE API (server)
export function getOAuthLoginErrorRedirectUrl(error: unknown): URL {
  if (error instanceof HttpError) {
    const errorMessage = isHttpErrorWithExtraMessage(error)
      ? `${error.message}: ${error.data.message}`
      : error.message;
    return makeOAuthLoginErrorUrl(errorMessage)
  }
  console.error("Unknown OAuth error:", error);
  return makeOAuthLoginErrorUrl("An unknown error occurred while trying to log in with the OAuth provider.");
}

// PRIVATE API (SDK)
export function getProviderCallbackUrl(providerName: string): URL {
  return new URL(appDelivery.waspApiUrl(`/auth/${providerName}/${providerCallbackPath}`));
}

function makeOAuthLoginErrorUrl(error: string): URL {
  const resultUrl = getOAuthLoginResultUrl()
  resultUrl.searchParams.set('error', error)
  return resultUrl
}

function isHttpErrorWithExtraMessage(error: HttpError): error is HttpError & { data: { message: string } } {
  return !!error.data && typeof (error.data as any).message === 'string';
}
