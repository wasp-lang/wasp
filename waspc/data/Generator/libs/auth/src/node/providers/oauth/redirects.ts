import { HttpError } from "@wasp.sh/lib-sdk-core";

export type OAuthRedirectsConfig = {
  frontendUrl: string;
  serverUrl: string;
  /** Path of the login route under `/auth/<provider>`. */
  loginPath: string;
  /** Path of the callback route under `/auth/<provider>`. */
  callbackPath: string;
  /** Path of the one-time code exchange route under `/auth`. */
  exchangeCodeForTokenPath: string;
  /** Client route the OAuth flow ends on. */
  clientOAuthCallbackPath: string;
};

export type OAuthRedirects = {
  loginPath: string;
  callbackPath: string;
  exchangeCodeForTokenPath: string;
  getRedirectUriForOneTimeCode: (oneTimeCode: string) => URL;
  handleOAuthErrorAndGetRedirectUri: (error: unknown) => URL;
  getRedirectUriForCallback: (providerName: string) => URL;
};

/** Creates the URL helpers the OAuth flow redirects through. */
export function createOAuthRedirects(
  config: OAuthRedirectsConfig,
): OAuthRedirects {
  const { frontendUrl, serverUrl, callbackPath, clientOAuthCallbackPath } =
    config;

  function getRedirectUriForOneTimeCode(oneTimeCode: string): URL {
    return new URL(`${frontendUrl}${clientOAuthCallbackPath}#${oneTimeCode}`);
  }

  function handleOAuthErrorAndGetRedirectUri(error: unknown): URL {
    if (error instanceof HttpError) {
      const errorMessage = isHttpErrorWithExtraMessage(error)
        ? `${error.message}: ${error.data.message}`
        : error.message;
      return getRedirectUriForError(errorMessage);
    }
    console.error("Unknown OAuth error:", error);
    return getRedirectUriForError(
      "An unknown error occurred while trying to log in with the OAuth provider.",
    );
  }

  function getRedirectUriForCallback(providerName: string): URL {
    return new URL(`${serverUrl}/auth/${providerName}/${callbackPath}`);
  }

  function getRedirectUriForError(error: string): URL {
    return new URL(`${frontendUrl}${clientOAuthCallbackPath}?error=${error}`);
  }

  return {
    loginPath: config.loginPath,
    callbackPath,
    exchangeCodeForTokenPath: config.exchangeCodeForTokenPath,
    getRedirectUriForOneTimeCode,
    handleOAuthErrorAndGetRedirectUri,
    getRedirectUriForCallback,
  };
}

function isHttpErrorWithExtraMessage(
  error: HttpError,
): error is HttpError & { data: { message: string } } {
  return !!error.data && typeof (error.data as any).message === "string";
}
