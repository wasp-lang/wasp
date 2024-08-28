import { config, HttpError } from '../../index.js';
// PRIVATE API (server)
export const loginPath = 'login';
// PRIVATE API (server)
export const exchangeCodeForTokenPath = 'exchange-code';
// PRIVATE API (server)
export const callbackPath = 'callback';
const clientOAuthCallbackPath = '/oauth/callback';
// PRIVATE API (server)
export function getRedirectUriForOneTimeCode(oneTimeCode) {
    return new URL(`${config.frontendUrl}${clientOAuthCallbackPath}#${oneTimeCode}`);
}
// PRIVATE API (server)
export function handleOAuthErrorAndGetRedirectUri(error) {
    if (error instanceof HttpError) {
        const errorMessage = isHttpErrorWithExtraMessage(error)
            ? `${error.message}: ${error.data.message}`
            : error.message;
        return getRedirectUriForError(errorMessage);
    }
    console.error("Unknown OAuth error:", error);
    return getRedirectUriForError("An unknown error occurred while trying to log in with the OAuth provider.");
}
// PRIVATE API (SDK)
export function getRedirectUriForCallback(providerName) {
    return new URL(`${config.serverUrl}/auth/${providerName}/${callbackPath}`);
}
function getRedirectUriForError(error) {
    return new URL(`${config.frontendUrl}${clientOAuthCallbackPath}?error=${error}`);
}
function isHttpErrorWithExtraMessage(error) {
    return error.data && typeof error.data.message === 'string';
}
//# sourceMappingURL=redirect.js.map