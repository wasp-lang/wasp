import {
  Response as ExpressResponse,
  Request as ExpressRequest,
} from 'express';
import * as arctic from 'arctic';

import type { ProviderConfig } from 'wasp/auth/providers/types';
import { HttpError } from 'wasp/server';

import { setOAuthCookieValue, getOAuthCookieValue } from './cookies.js';

export type OAuthStateFor<
  OT extends OAuthType
> = OAuthStateForOAuthType[OT];

export type OAuthStateWithCodeFor<OT extends OAuthType> = OAuthStateFor<OT> & OAuthCode

export type OAuthType = keyof OAuthStateForOAuthType;

export type OAuthStateFieldName = keyof OAuthState | keyof OAuthStateWithPKCE;

type OAuthStateForOAuthType = {
  OAuth2: OAuthState,
  OAuth2WithPKCE: OAuthStateWithPKCE,
};

type OAuthState = {
  state: string;
};

type OAuthStateWithPKCE = {
  state: string;
  codeVerifier: string;
};

/**
 * When the OAuth flow is completed, the OAuth provider will redirect the user back to the app
 * with a code. This code is then exchanged for an access token.
 */
type OAuthCode = {
  code: string;
};

export function generateAndStoreOAuthState<OT extends OAuthType>({
  oAuthType,
  provider,
  res,
}: {
  oAuthType: OT,
  provider: ProviderConfig,
  res: ExpressResponse
}): OAuthStateFor<OT> {
  const state: OAuthStateFor<OT> = {
    ...generateState(),
    ...(oAuthType === 'OAuth2WithPKCE' && generateCodeVerifier()),
  };

  storeOAuthState(provider, res, state);

  return state;
}

export function validateAndGetOAuthState<OT extends OAuthType>({
  oAuthType,
  provider,
  req,
}: {
  oAuthType: OT,
  provider: ProviderConfig,
  req: ExpressRequest
}): OAuthStateWithCodeFor<OT> {
  const state: OAuthStateFor<OT> = {
    ...getState(req),
    ...(oAuthType === 'OAuth2WithPKCE' && getCodeVerifier(provider, req)),
  };

  // We validate the state before reading anything else from the callback,
  // so only a callback for a login this browser started gets handled.
  validateOAuthState(provider, req, state);

  return { ...state, ...getCode(provider, req) };
}

function storeOAuthState(
  provider: ProviderConfig,
  res: ExpressResponse,
  state: OAuthStateFor<OAuthType>
): void {
  let key: keyof typeof state;
  for (key in state) {
    setOAuthCookieValue(provider, res, key, state[key]);
  }
}

function validateOAuthState(
  provider: ProviderConfig,
  req: ExpressRequest,
  state: OAuthStateFor<OAuthType>
): void {
  // This fails when the login outlived the cookies, was finished in a
  // different browser, or was replaced by a newer login attempt.
  const storedState = getOAuthCookieValue(provider, req, 'state');
  if (!state.state || !storedState || storedState !== state.state) {
    throw createLoginExpiredError(provider);
  }

  if (isOAuthStateWithPKCE(state) && !state.codeVerifier) {
    throw createLoginExpiredError(provider);
  }
}

function createLoginExpiredError(provider: ProviderConfig): HttpError {
  return new HttpError(
    400,
    `Your login attempt with ${provider.displayName} expired. Please try again.`
  );
}

function generateState(): { state: string } {
  return { state: arctic.generateState() };
}

function generateCodeVerifier(): { codeVerifier: string } {
  return { codeVerifier: arctic.generateCodeVerifier() };
}

function getCode(
  provider: ProviderConfig,
  req: ExpressRequest
): { code: string } {
  const { code, error, error_description } = req.query;

  // The provider's error is meant for developers, so we only log it (the
  // handler logs `data`) and never show it to the user.
  const providerErrorDetails = {
    providerError: error,
    providerErrorDescription: error_description,
  };

  // Providers send `access_denied` when the user cancels on the consent screen,
  // but some (e.g. Keycloak, Auth0) also send it when their own policy denies
  // the login.
  if (error === 'access_denied') {
    throw new HttpError(
      400,
      `Login with ${provider.displayName} was cancelled or denied.`,
      providerErrorDetails
    );
  }

  if (error !== undefined || typeof code !== 'string' || code === '') {
    throw new HttpError(
      400,
      `Unable to log in with ${provider.displayName}. Please try again later.`,
      providerErrorDetails
    );
  }

  return { code };
}

function getState(req: ExpressRequest): { state: string } {
  return { state:  `${req.query.state}` };
}

function getCodeVerifier(
  provider: ProviderConfig,
  req: ExpressRequest
): { codeVerifier: string } {
  const codeVerifier = getOAuthCookieValue(
    provider,
    req,
    'codeVerifier'
  );
  // The cookie can be missing (dropped by the browser, or expired while the user
  // sat on the consent screen). `validateOAuthState` rejects an empty code
  // verifier, so we normalize the missing case into one instead of lying about
  // the type.
  return { codeVerifier: codeVerifier ?? '' };
}

function isOAuthStateWithPKCE(
  state: OAuthState | OAuthStateWithPKCE
): state is OAuthStateWithPKCE {
  return 'codeVerifier' in state;
}
