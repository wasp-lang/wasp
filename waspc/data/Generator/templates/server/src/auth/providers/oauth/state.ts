import {
  Response as ExpressResponse,
  Request as ExpressRequest,
} from 'express';
import { generateCodeVerifier, generateState } from 'arctic';

import type { ProviderConfig } from 'wasp/auth/providers/types';

import { setOAuthCookieValue, getOAuthCookieValue } from './cookies.js';

/**
 * OAuth state shape depends on whether the provider uses PKCE.
 * If a provider uses PKCE, the state will include a code verifier.
 */
export type OAuthState<UsesCodeVerifier extends boolean = false> = {
  state: string;
} & (UsesCodeVerifier extends true
  ? { codeVerifier: string }
  : {});

export type OAuthStateType = keyof OAuthState<true>;

export function generateAndStoreOAuthState<IsCodeVerifierGenerated extends boolean>(
  isCodeVerifierGenerated: IsCodeVerifierGenerated,
  provider: ProviderConfig,
  res: ExpressResponse
): OAuthState<IsCodeVerifierGenerated> {
  const state = generateState();
  setOAuthCookieValue(provider, res, 'state', state);

  return {
    state,
    ...(isCodeVerifierGenerated && generateAndSetCodeVerifier(provider, res)),
  };
}

export function validateAndGetOAuthState<IsCodeVerifierValidated extends boolean>(
  isCodeVerifierValidated: IsCodeVerifierValidated,
  provider: ProviderConfig,
  req: ExpressRequest
): OAuthState<IsCodeVerifierValidated> & {
  code: string;
} {
  const code = req.query.code;
  if (typeof code !== 'string') {
    throw new Error('Invalid code');
  }

  const state = req.query.state;
  const storedState = getOAuthCookieValue(provider, req, 'state');
  if (!state || !storedState || storedState !== state) {
    throw new Error('Invalid state');
  }

  return {
    state,
    code,
    ...(isCodeVerifierValidated && validateAndGetCodeVerifier(provider, req)),
  };
}

function generateAndSetCodeVerifier(
  provider: ProviderConfig,
  res: ExpressResponse
) {
  const codeVerifier = generateCodeVerifier();
  setOAuthCookieValue(provider, res, 'codeVerifier', codeVerifier);

  return { codeVerifier };
}

function validateAndGetCodeVerifier(
  provider: ProviderConfig,
  req: ExpressRequest
) {
  const storedCodeVerifier = getOAuthCookieValue(
    provider,
    req,
    'codeVerifier'
  );
  if (!storedCodeVerifier) {
    throw new Error('Invalid code verifier');
  }
  return {
    codeVerifier: storedCodeVerifier,
  };
}