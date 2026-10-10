import { config } from '../../index.js'
import { HttpError } from '../../../errors/index.js'
import { reportServerError } from "../../errors.js";

// PRIVATE API (server)
export const loginPath = 'login'

// PRIVATE API (server)
export const exchangeCodeForTokenPath = 'exchange-code'

// PRIVATE API (server)
export const callbackPath = 'callback'

const clientOAuthCallbackPath = '/oauth/callback'

// PRIVATE API (server)
export function getRedirectUriForOneTimeCode(oneTimeCode: string): URL {
  return new URL(`${config.frontendUrl}${clientOAuthCallbackPath}#${oneTimeCode}`);
}

// PRIVATE API (server)
export function handleOAuthErrorAndGetRedirectUri(error: unknown): URL {
  if (error instanceof HttpError) {
    const errorMessage = isHttpErrorWithExtraMessage(error)
      ? `${error.message}: ${error.data.message}`
      : error.message;
    return getRedirectUriForError(errorMessage)
  }
  reportServerError(error, { source: "oauth" });
  return getRedirectUriForError("An unknown error occurred while trying to log in with the OAuth provider.");
}

// PRIVATE API (SDK)
export function getRedirectUriForCallback(providerName: string): URL {
  return new URL(`${config.serverUrl}/auth/${providerName}/${callbackPath}`);
}

function getRedirectUriForError(error: string): URL {
  const url = new URL(`${config.frontendUrl}${clientOAuthCallbackPath}`);
  url.searchParams.set("error", error);
  return url;
}

function isHttpErrorWithExtraMessage(error: HttpError): error is HttpError & { data: { message: string } } {
  return !!error.data && typeof (error.data as any).message === 'string';
}
