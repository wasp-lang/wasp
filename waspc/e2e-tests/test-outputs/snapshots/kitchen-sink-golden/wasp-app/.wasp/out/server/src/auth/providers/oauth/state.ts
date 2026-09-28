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
  const state: OAuthStateWithCodeFor<OT> = {
    ...getCode(req),
    ...getState(req),
    ...(oAuthType === 'OAuth2WithPKCE' && getCodeVerifier(provider, req)),
  };

  validateOAuthState(provider, req, state);

  return state;
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
  state: OAuthStateWithCodeFor<OAuthType>
): void {
  // `getCode` throws at extraction when the code is absent, non-string, or
  // empty, so a non-empty string is guaranteed here. Authorization codes are
  // opaque (RFC 6749, Appendix A.11), so no string value is rejected here.
  if (typeof state.code !== 'string' || state.code === '') {
    throw new HttpError(400, 'Unable to login with the OAuth provider. The authorization code is missing or invalid.');
  }

  const storedState = getOAuthCookieValue(provider, req, 'state');
  if (!state.state || !storedState || storedState !== state.state) {
    throw new HttpError(400, 'Unable to login with the OAuth provider. The state is invalid.');
  }

  if (isOAuthStateWithPKCE(state) && !state.codeVerifier) {
    throw new HttpError(400, 'Unable to login with the OAuth provider. The code verifier is missing.');
  }
}

function generateState(): { state: string } {
  return { state: arctic.generateState() };
}

function generateCodeVerifier(): { codeVerifier: string } {
  return { codeVerifier: arctic.generateCodeVerifier() };
}

function getCode(req: ExpressRequest): { code: string } {
  const code = req.query.code;
  // Reject absent, non-string, or empty codes at extraction instead of
  // stringifying them: `undefined` is a valid opaque authorization code
  // (RFC 6749, Appendix A.11), so no real value can be reserved as a sentinel.
  if (typeof code !== 'string' || code === '') {
    throw new HttpError(400, 'Unable to login with the OAuth provider. The authorization code is missing or invalid.');
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
