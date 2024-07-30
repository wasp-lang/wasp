{{={= =}=}}
import { config } from 'wasp/server'
import { HttpError } from 'wasp/server'

export const loginPath = '{= serverOAuthLoginHandlerPath =}'
export const exchangeCodeForTokenPath = '{= serverExchangeCodeForTokenHandlerPath =}'
const clientOAuthCallbackPath = '{= clientOAuthCallbackPath =}'

export function getRedirectUriForOneTimeCode(oneTimeCode: string): URL {
  return new URL(`${config.frontendUrl}${clientOAuthCallbackPath}#${oneTimeCode}`);
}

export function handleOAuthErrorAndGetRedirectUri(error: unknown): URL {
  if (error instanceof HttpError) {
    const errorMessage = isHttpErrorWithExtraMessage(error)
      ? `${error.message}: ${error.data.message}`
      : error.message;
    return getRedirectUriForError(errorMessage)
  }
  console.error("Unknown OAuth error:", error);
  return getRedirectUriForError("An unknown error occurred while trying to log in with the OAuth provider.");
}

function getRedirectUriForError(error: string): URL {
  return new URL(`${config.frontendUrl}${clientOAuthCallbackPath}?error=${error}`);
}

function isHttpErrorWithExtraMessage(error: HttpError): error is HttpError & { data: { message: string } } {
  return error.data && typeof (error.data as any).message === 'string';
}
