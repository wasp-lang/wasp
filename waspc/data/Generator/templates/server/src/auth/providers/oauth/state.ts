import {
  Response as ExpressResponse,
  Request as ExpressRequest,
} from 'express';
import { generateCodeVerifier, generateState } from 'arctic';

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
  return {
    ...generateAndStoreState(provider, res),
    ...(oAuthType === 'OAuth2WithPKCE' && generateAndStoreCodeVerifier(provider, res)),
  };
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
  return {
    ...validateAndGetCode(req),
    ...validateAndGetState(provider, req),
    ...(oAuthType === 'OAuth2WithPKCE' && validateAndGetCodeVerifier(provider, req)),
  };
}

function generateAndStoreState(
  provider: ProviderConfig,
  res: ExpressResponse
): { state: string } {
  const state = generateState();
  setOAuthCookieValue(provider, res, 'state', state);

  return { state };
}

function generateAndStoreCodeVerifier(
  provider: ProviderConfig,
  res: ExpressResponse
): { codeVerifier: string } {
  const codeVerifier = generateCodeVerifier();
  setOAuthCookieValue(provider, res, 'codeVerifier', codeVerifier);

  return { codeVerifier };
}

function validateAndGetCode(req: ExpressRequest): { code: string } {
  const code = req.query.code;
  if (typeof code !== 'string') {
    throw new Error('Invalid code');
  }
  return { code };
}

function validateAndGetState(
  provider: ProviderConfig,
  req: ExpressRequest
): { state: string } {
  const state = req.query.state;
  const storedState = getOAuthCookieValue(provider, req, 'state');
  if (!state || !storedState || storedState !== state) {
    throw new Error('Invalid state');
  }
  return { state };
}

function validateAndGetCodeVerifier(
  provider: ProviderConfig,
  req: ExpressRequest
): { codeVerifier: string } {
  const codeVerifier = getOAuthCookieValue(
    provider,
    req,
    'codeVerifier'
  );
  if (!codeVerifier) {
    throw new Error('Missing code verifier');
  }
  return { codeVerifier };
}
