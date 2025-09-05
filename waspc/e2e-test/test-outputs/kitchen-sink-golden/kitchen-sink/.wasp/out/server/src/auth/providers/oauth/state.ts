import {
  Response as ExpressResponse,
  Request as ExpressRequest,
} from 'express';
import * as arctic from 'arctic';

import type { ProviderConfig } from 'wasp/auth/providers/types';

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
  if (typeof state.code !== 'string') {
    throw new Error('Invalid code');
  }

  const storedState = getOAuthCookieValue(provider, req, 'state');
  if (!state.state || !storedState || storedState !== state.state) {
    throw new Error('Invalid state');
  }

  if (isOAuthStateWithPKCE(state) && !state.codeVerifier) {
    throw new Error('Missing code verifier');
  }
}

function generateState(): { state: string } {
  return { state: arctic.generateState() };
}

function generateCodeVerifier(): { codeVerifier: string } {
  return { codeVerifier: arctic.generateCodeVerifier() };
}

function getCode(req: ExpressRequest): { code: string } {
  return { code: `${req.query.code}` };
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
  return { codeVerifier };
}

function isOAuthStateWithPKCE(
  state: OAuthState | OAuthStateWithPKCE
): state is OAuthStateWithPKCE {
  return 'codeVerifier' in state;
}
